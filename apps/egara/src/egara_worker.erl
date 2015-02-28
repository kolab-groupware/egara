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
    %% EventMapping is a map of event types (e.g. <<"FlagsClear">>) to the type of
    %% event (e.g. imap_mesage_event) for routing the notifications through the handler
    EventMapping = transform_events_config_to_dict(application:get_env(events_to_track)),
    %%lager:info("We gots us ... ~p", [EventMapping]),
    { ok, EventMapping }.

handle_call(_Request, _From, State) ->
    { reply, ok, State }.

handle_cast(process_events, State) ->
    case poolboy:checkout(egara_storage_pool, false, 10) of
         Storage when is_pid(Storage) ->
             %%lager:info("Storing using ~p", [Storage]),
            process_as_many_events_as_possible(Storage, State, ?BATCH_SIZE),
            poolboy:checkin(egara_storage_pool, Storage);
         _ ->
            lager:warning("Unable to get storage!")
    end,
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
add_events_to_dict(Type, Events, EventMap) when is_list(Events) ->
    F = fun(Event, Map) ->
                case dict:is_key(Event, Map) of
                   true -> dict:update(Event, Type, Map);
                   _ -> dict:store(Event, Type, Map)
                end
            end,
    lists:foldl(F, EventMap, Events);
add_events_to_dict(_Type, _Events, EventMap) ->
    EventMap.

transform_events_config_to_dict({ ok, EventConfig }) ->
    lists:foldl(fun({ Type, Events }, EventMap) -> add_events_to_dict(Type, Events, EventMap) end, dict:new(), EventConfig);
transform_events_config_to_dict(_) ->
    dict:new(). %% return an empty map .. this is going to be boring

process_as_many_events_as_possible(_Storage, _EventMapping, 0) ->
    egara_notifications_processor:queue_drained(),
    ok;
process_as_many_events_as_possible(Storage, EventMapping, N) ->
    Status = egara_notification_queue:assign_next(self()),
    %%lager:info("~p is starting to process... ~p", [self(), Key]),
    case notification_assigned(Storage, EventMapping, Status) of
        again -> process_as_many_events_as_possible(Storage, EventMapping, N - 1);
        _ -> ok
    end.

notification_assigned(_Storage, _EventMapping, notfound) ->
    %%lager:info("Checking in ~p", [self()]),
    poolboy:checkin(egara_notification_workers, self()),
    egara_notifications_processor:queue_drained(),
    done;
notification_assigned(Storage, EventMapping, { Key, Notification } ) ->
    EventType = proplists:get_value(<<"event">>, Notification),
    EventCategory = dict:find(EventType, EventMapping),
    %%lager:info("Type is ~p which maps to ~p", [EventType, EventCategory]),
    Result = process_notification_by_category(Storage, Notification, EventCategory),
    post_process_event(Key, Result).

post_process_event(Key, ok) ->
    %%lager:info("Done with ~p", [Key]),
    egara_notification_queue:remove(Key),
    again;
post_process_event(Key, ignoring) ->
    %%lager:info("Ignoring ~p", [Key]),
    egara_notification_queue:remove(Key),
    again;
post_process_event(_, _) ->
    error.

process_notification_by_category(Storage, Notification, { ok, Type }) ->
    %% this version, with the { ok, _ } tuple is called due to maps:find returning { ok, Value }
    %% it is essentiall a forwarder to other impls below
    NotificationWithUsername = ensure_username(Storage, Notification, proplists:get_value(<<"user">>, Notification)),
    process_notification_by_category(Storage, NotificationWithUsername, Type);
process_notification_by_category(Storage, Notification, imap_message_event) ->
    %%lager:info("storing an imap_message_event"),
    Key = <<"TODO">>, %% TODO!
    egara_storage:store_notification(Storage, Key, Notification);
process_notification_by_category(Storage, Notification, imap_mailbox_event) ->
    %%lager:info("storing an imap_mailbox_event"),
    Key = <<"TODO">>, %% TODO!
    egara_storage:store_notification(Storage, Key, Notification);
process_notification_by_category(Storage, Notification, imap_session_event) ->
    %%lager:info("storing an imap_session_event"),
    Key = <<"TODO">>, %% TODO!
    egara_storage:store_notification(Storage, Key, Notification);
process_notification_by_category(Storage, Notification, imap_quota_event) ->
    %%lager:info("storing an imap_quota_event"),
    Key = <<"TODO">>, %% TODO!
    egara_storage:store_notification(Storage, Key, Notification);
process_notification_by_category(_Storage, _Notification, _) ->
    %% in here we have a notification we don't recognize, probably because it was not configured
    %% to be watched for
    ignoring.

ensure_username(_Storage, Notification, undefined) ->
    Notification;
ensure_username(Storage, Notification, UserLogin) ->
    FromStorage = egara_storage:fetch_userdata_for_login(Storage, UserLogin),
    %%lager:info("Storage said ... ~p", [FromStorage]),
    add_username_from_storage(Storage, Notification, UserLogin, FromStorage).

add_username_from_storage(Storage, Notification, UserLogin, notfound) ->
    LDAP = poolboy:checkout(egara_ldap_pool, false, 10),
    RV = query_ldap_for_username(Storage, Notification, UserLogin, LDAP),
    poolboy:checkin(egara_ldap_pool, LDAP),
    RV;
add_username_from_storage(_Storage, Notification, _UserLogin, UserData) ->
    [ { <<"user_id">>, proplists:get_value(<<"id">>, UserData, <<"">>) } | Notification ].

query_ldap_for_username(Storage, Notification, UserLogin, LDAP) when is_pid(LDAP) ->
    FromLDAP = egara_storage:fetch_userdata_for_login(LDAP, UserLogin),
    add_username_from_ldap(Storage, Notification, UserLogin, FromLDAP);
query_ldap_for_username(_Storage, Notification, _UserLogin, _) ->
    lager:warning("Unable to get an LDAP worker"),
    Notification.

add_username_from_ldap(_Storage, Notification, _UserLogin, notfound) ->
    %%lager:info("LDAP said notfound"),
    Notification;
add_username_from_ldap(Storage, Notification, UserLogin, UserData) ->
    %%lager:info("LDAP gave us back ... ~p", [UserData]),
    egara_storage:store_userdata(Storage, UserLogin, UserData),
    UserIdTuple = { <<"user_id">>, proplists:get_value(<<"id">>, UserData, <<"">>) },
    [ UserIdTuple | Notification ].
