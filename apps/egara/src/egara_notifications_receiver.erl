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

%% API
-export([ start_link/0
        , notification_received/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { storage_id = 0, processor_notifier_pid }).
-define(NOTIFICATION_BATCH_SIZE, 500).

%% API
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
notification_received(NotificationJson) -> gen_server:cast(?MODULE, { notification, NotificationJson }).

%% private/internal functions
inform_notifications_processor(N) when is_number(N), N >= ?NOTIFICATION_BATCH_SIZE ->
    egara_notifications_processor:process_backlog(),
    0;
inform_notifications_processor(N) ->
    N.

%%TODO this process could be started only if needed, killed when not?
notifications_processor_notifier(Total) when is_number(Total) ->
    receive
        Pending when is_number(Pending) -> notifications_processor_notifier(inform_notifications_processor(Total + Pending))
    after 1000 ->
        if Total > 0 -> inform_notifications_processor(?NOTIFICATION_BATCH_SIZE);
           true -> ok
        end,
        notifications_processor_notifier(0)
    end.

start_receiver([]) -> ok;
start_receiver([cyrus|Tail]) -> lager:info("Starting receiver: cyrus"), egara_incoming_cyrus_imap:start_reception(), start_receiver(Tail);
start_receiver([_|Tail]) -> start_receiver(Tail).

start_notification_reception() ->
    case application:get_env(receivers) of
        { ok, Receivers } when is_list(Receivers) -> start_receiver(Receivers);
        _ -> ok
    end.

%% gen_server callbacks
init([]) ->
    MaxKey = egara_notification_store:max_key(),
    Rv = start_notification_reception(),
    %%TODO: on Rv = error, do something appropriate
    lager:info("Notification reception started ... ~p", [Rv]),
    { ok, #state{ storage_id = MaxKey + 1, processor_notifier_pid = spawn(fun() -> notifications_processor_notifier(0) end) } }.

handle_call(_, _From, State) ->
    { reply, ok, State }.

handle_cast({ notification, Notification }, State) ->
    egara_notification_store:add(State#state.storage_id, Notification),
    State#state.processor_notifier_pid ! 1,
    { noreply, State#state{ storage_id = State#state.storage_id + 1 } }; %% if paralellized, this needs to be syncronized

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
