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

-module(egara_worker).

-behaviour(gen_server).

%% API
-export([ start_link/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    { ok, [] }.

handle_call(_Request, _From, State) ->
    { reply, ok, State }.

handle_cast(process_events, State) ->
    Key = egara_notification_store:assign_next(self()),
    %%lager:info("~p is starting to process... ~p", [self(), Key]),
    notification_assigned(Key),
    { noreply, State };

handle_cast(_Msg, State) ->
    { noreply, State }.

handle_info(_Info, State) ->
    { noreply, State }.

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

%% private API
notification_assigned(notfound) ->
    %%lager:info("Checking in ~p", [self()]),
    poolboy:checkin(egara_notification_workers, self()),
    egara_notifications_processor:queue_drained();
notification_assigned(Key) ->
    egara_notification_store:remove(Key),
    gen_server:cast(self(), process_events),
    %%lager:info("Done with ~p", [Key]),
    ok.

