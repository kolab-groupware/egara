%% Copyright 2014 Kolab Systems AG (http://www.kolabsys.com)
%%
%% Aaron Seigo (Kolab Systems) <seigo a kolabsys.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(egara_notifications_receiver).

-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").

%% API
-export([ start_link/0
        , notification_received/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { storage_id = 0 }).
-define(PF_LOCAL, 1).
-define(SOCK_DGRAM, 2).
-define(UNIX_PATH_MAX, 108).
-define(MAX_SLEEP_MS, 1000).
-define(MIN_SLEEP_MS, 1).

%% API

start_link()    -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
notification_received(Notification) -> gen_server:cast(?MODULE, { notification, Notification }).


%% gen_server callbacks
init([]) ->
    %% get the path to the listen socket, either from the app config
    %% or here
    case application:get_env(notification_socket_path) of
        Value when is_binary(Value) -> SocketPath = Value;
        _ -> SocketPath = <<"/tmp/egara-notify">>
    end,

    %% see if the file exists, and if it does, remove it if it is a socket
    %% allows to start the application multiple times, which is a good thing
    case file:read_file_info(SocketPath) of
        { ok, #file_info{ type = other } } -> ok = file:delete(SocketPath); %% TODO: handle errror with a report
        { ok, _ } -> notok; %% do not remove a non-socket file. TODO: handle errror with a report, clean exit
        {error, _} -> ok
    end,

    { ok, Socket } = procket:socket(?PF_LOCAL, ?SOCK_DGRAM, 0),
    Sun = <<?PF_LOCAL:16/native, % sun_family
            SocketPath/binary,   % address
            0:((?UNIX_PATH_MAX-byte_size(SocketPath))*8) %% zero out the rest
          >>,

    case procket:bind(Socket, Sun) of
        ok -> spawn(fun() -> recvNotification(Socket, ?MIN_SLEEP_MS) end);
        { error, PosixError } -> lager:error("Could not bind to notification socket; error is: ~p", [PosixError])
    end,

    MaxKey = egara_notification_store:max_key(),
    { ok, #state{ storage_id = MaxKey + 1 } }.

%% silly little thing that grabs the second-to-last item; very specific to what cyrus throws at us
%% TODO: could be made more efficient by just going N items in since we "know" the format?
cherryPickNotification([], LastTerm, _) ->
    LastTerm;
cherryPickNotification(Terms, _, ThisTerm) ->
    [H|T] = Terms,
    cherryPickNotification(T, ThisTerm, H).
cherryPickNotification(Terms) ->
    [H|T] = Terms,
    cherryPickNotification(T, null, H).

recvNotification(Socket, SleepMs) ->
    case procket:recvfrom(Socket, 16#FFFF) of
        { error, eagain } ->
            NewSleepMs = min(SleepMs * 2, ?MAX_SLEEP_MS),
            timer:sleep(NewSleepMs),
            recvNotification(Socket, NewSleepMs);
        { ok, Buf } ->
            %%lager:info("~s", [binary_to_list(Buf)]),
            Components = binary:split(Buf, <<"\0">>, [global]),
            %%lager:info("~p", [Components]),
            Json = cherryPickNotification(Components),
            gen_server:cast(?MODULE, { notification, Json }),
            recvNotification(Socket, ?MIN_SLEEP_MS)
    end.

handle_call(_, _From, State) ->
    { reply, ok, State }.

handle_cast({ notification, Notification }, State) when is_binary(Notification) ->
    try jsx:decode(Notification) of
        Term -> egara_notification_store:add(State#state.storage_id, Term),
                { noreply, State#state{ storage_id = State#state.storage_id + 1 } } %% if paralellized, this needs to be syncronized
    catch
        error:_ -> { noreply, State }
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Upgrade from 2
code_change(_OldVsn, State, [from1To2]) ->
    error_logger:info_msg("CODE_CHANGE from 2~n"),
    { state, StorageId } = State,
    NewState = #state{ storage_id = StorageId  },
    { ok, NewState }.

%% Note downgrade code_change not implemented
    
    

%%% Internal functions
