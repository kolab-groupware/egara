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

-module(egara_notifications_processor).
-behaviour(gen_server).

%% API
-export([ start_link/0,
          notifications_received/0,
          process_backlog/0,
          queue_drained/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(BACKLOG_BATCHSIZE, 1).

%% API
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
notifications_received() -> gen_server:cast(?MODULE, { notifications_received }).
process_backlog() -> gen_server:cast(?MODULE, { process_backlog }).
queue_drained() -> process_backlog().

%% private/internal functions
process_backlog_if_notifications(0) ->
    lager:info("NO NOTIFICATIONS!"),
    ok;
process_backlog_if_notifications(N) when is_number(N) ->
    start_a_worker();
process_backlog_if_notifications(_) ->
    lager:info("GOT SOMETHING WE DIDN'T EXPECT?"),
    ok.
process_backlog_if_notifications() ->
    case egara_notification_store:next_unnasigned() of
        none -> process_backlog_if_notifications(egara_notification_store:release_orphaned());
        error -> error;
        _ -> process_backlog_if_notifications(1) % we don't really know how many, but there is at least 1
    end.

start_a_worker() ->
    try
        case poolboy:checkout(egara_notification_workers, false, 10) of
            Worker when is_pid(Worker) ->
                %%lager:info("Checked out ~p", [Worker]),
                gen_server:cast(Worker, process_events),
                start_a_worker();
            _ ->
                ok
        end
    catch
        _ ->
            ok
    end.


%% gen_server callbacks
init([]) ->
    process_backlog(),
    lager:info("Notification processing started ... "),
    { ok, [] }.

handle_call(_, _From, State) ->
    { reply, ok, State }.

handle_cast({ notifications_received}, State) ->
    { noreply, State };

handle_cast({ process_backlog}, State) ->
    lager:info("Handling backlog...."),
    process_backlog_if_notifications(),
    { noreply, State };

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Upgrade from 2
code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

%% Note downgrade code_change not implemented
    
    

%%% Internal functions
