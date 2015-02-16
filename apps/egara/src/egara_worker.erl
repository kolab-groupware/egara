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
-define(BATCH_SIZE, 500).

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    { ok, ImapConfig } = application:get_env(imap_events),
    { ok, ImapConfig }.

handle_call(_Request, _From, State) ->
    { reply, ok, State }.

handle_cast(process_events, State) ->
    process_as_many_events_as_possible(?BATCH_SIZE),
    { noreply, State };

handle_cast(_Msg, State) ->
    { noreply, State }.

handle_info(_Info, State) ->
    { noreply, State }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

%% private API
process_as_many_events_as_possible(0) ->
    egara_notifications_processor:queue_drained(),
    ok;
process_as_many_events_as_possible(N) ->
    Status = egara_notification_queue:assign_next(self()),
    %%lager:info("~p is starting to process... ~p", [self(), Key]),
    case notification_assigned(Status) of
        again -> process_as_many_events_as_possible(N - 1);
        _ -> ok
    end.

notification_assigned(notfound) ->
    %%lager:info("Checking in ~p", [self()]),
    poolboy:checkin(egara_notification_workers, self()),
    egara_notifications_processor:queue_drained(),
    done;
notification_assigned({ Key, Notification }) ->
    %%TODO actual processing
    case poolboy:checkout(egara_storage_pool, false, 10) of
         Storage when is_pid(Storage) ->
             %%lager:info("Storing using ~p", [Storage]),
             egara_storage:store_notification(Storage, Key, Notification),
             egara_notification_queue:remove(Key),
             poolboy:checkin(egara_storage_pool, Storage),
             ok;
         _ -> 
            lager:warning("Unable to get storage!"),
            egara_notification_queue:release(Key, self()),
            no_storage
    end,
    %%lager:info("Done with ~p", [Key]),
    again.

