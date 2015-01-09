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
          process_notification/2,
          start_work_after_assigned/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
notifications_received() -> gen_server:cast(?MODULE, { notifications_received }).
process_backlog() -> gen_server:cast(?MODULE, { process_backlog }).

%% private/internal functions
start_work_after_assigned() ->
    receive
        { Key, _Term } ->
            %%TODO actual work!
            egara_notification_store:remove(Key),
            lager:info("Finished ~p", [Key]),
            ok;
        terminate ->
            ok;
        Val ->
            lager:warning("Got something unexpected during processing weird ~p", [Val]),
            error
    end.

process_notification(Key, Term) ->
    PID = spawn(fun egara_notifications_processor:start_work_after_assigned/0),
    case egara_notification_store:assign(Key, PID) of
        ok -> PID ! { Key, Term }, ok;
         V -> lager:info("Terminating ~p due to getting ~p", [PID, V]), PID ! terminate, notok
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
    egara_notification_store:process_next_unnasigned(50, fun egara_notifications_processor:process_notification/2),
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
