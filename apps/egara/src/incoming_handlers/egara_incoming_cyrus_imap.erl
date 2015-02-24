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

-module(egara_incoming_cyrus_imap).
-behavior(egara_incoming_handler).

%% API
-export([ start_reception/0, launchRecvCyrusNotification/1, recvCyrusNotification/3 ]).

-include_lib("kernel/include/file.hrl").

-define(PF_LOCAL, 1).
-define(SOCK_DGRAM, 2).
-define(UNIX_PATH_MAX, 108).
-define(MAX_SLEEP_MS, 500).
-define(MIN_SLEEP_MS, 1).

start_reception() ->
    %% get the path to the listen socket, either from the app config
    %% or here
    DefaultSocketPath = <<"/var/lib/imap/socket/notify">>,
    case application:get_env(cyrus) of
        { ok, Config } when is_list(Config) -> SocketPath = proplists:get_value(notification_socket_path, Config, DefaultSocketPath);
        _ -> SocketPath = DefaultSocketPath
    end,

    lager:info("Listening for cyrus-imap events on socket ~p", [SocketPath]),
    %% see if the file exists, and if it does, remove it if it is a socket
    %% allows to start the application multiple times, which is a good thing
    CleanUp =
    case file:read_file_info(SocketPath) of
        { ok, #file_info{ type = other } } -> ok = file:delete(SocketPath); %% TODO: handle errror with a report
        { ok, _ } -> error; %% do not remove a non-socket file. TODO: handle errror with a report, clean exit
        { error, _ } -> ok
    end,

    if CleanUp =:= ok -> bindSocket(SocketPath), ok;
       true -> error
    end.

bindSocket(SocketPath) ->
    { ok, Socket } = procket:socket(?PF_LOCAL, ?SOCK_DGRAM, 0),
    Sun = <<?PF_LOCAL:16/native, % sun_family
            SocketPath/binary,   % address
            0:((?UNIX_PATH_MAX-byte_size(SocketPath))*8) %% zero out the rest
          >>,

    case procket:bind(Socket, Sun) of
        ok -> spawn(fun() -> supervisor:start_child(egara_sup, { ?MODULE, { ?MODULE, launchRecvCyrusNotification, [Socket] }, permanent, 5000, worker, [?MODULE]}) end),
              ok;
        { error, PosixError } -> lager:error("Could not bind to notification socket; error is: ~p", [PosixError]), error
    end.

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


launchRecvCyrusNotification(Socket) ->
    % this mapping gets passed to proplists:expand to translate/normalize event names
    EventMappings = [
                     {
                       { <<"event">>, <<"vnd.cmu.MessageCopy">> },
                       { <<"event">>, <<"MessageCopy">>}
                     },
                     {
                       { <<"event">>, <<"vnd.cmu.MessageMove">> },
                       { <<"event">>, <<"MessageMove">>}
                     }
                    ],
    { ok, spawn_link(?MODULE, recvCyrusNotification, [Socket, EventMappings, ?MIN_SLEEP_MS])  }.

recvCyrusNotification(Socket, EventMappings, SleepMs) ->
    case procket:recvfrom(Socket, 16#FFFF) of
        { error, eagain } ->
            NewSleepMs = min(SleepMs * 2, ?MAX_SLEEP_MS),
            timer:sleep(NewSleepMs),
            recvCyrusNotification(Socket, EventMappings, NewSleepMs);
        { ok, Buf } ->
            Components = binary:split(Buf, <<"\0">>, [global]),
            %%lager:info("~p", [Components]),
            Json = cherryPickNotification(Components),
            try jsx:decode(Json) of
                Term -> egara_notifications_receiver:notification_received(proplists:expand(EventMappings, Term))
            catch
                error:_ -> ok
            end,

            recvCyrusNotification(Socket, EventMappings, ?MIN_SLEEP_MS)
    end.

