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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(BATCH_SIZE, 500).

-record(state, { event_mapping, storage, admin_user_prefix }).

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    %% EventMapping is a map of event types (e.g. <<"FlagsClear">>) to the type of
    %% event (e.g. imap_mesage_event) for routing the notifications through the handler
    EventMapping = transform_events_config_to_dict(application:get_env(events_to_track)),
    { ok, Storage } = egara_storage:start_link(),
    %% get the admin user, whose events we will ignore
    Config = application:get_env(egara, imap, []),
    AdminConnConfig = proplists:get_value(admin_connection, Config, []),
    AdminUser = list_to_binary(proplists:get_value(user, AdminConnConfig, "cyrus-admin")),
    AdminUserPrefix = <<AdminUser/binary, "@">>,
    State = #state{ event_mapping = EventMapping, storage = Storage, admin_user_prefix = AdminUserPrefix },
    { ok, State }.

handle_call(_Request, _From, State) ->
    { reply, ok, State }.

handle_cast(process_events, State) ->
    process_as_many_events_as_possible(State, ?BATCH_SIZE),
    { noreply, State };

handle_cast(_Msg, State) ->
    { noreply, State }.

handle_info({ { imap_mailbox_metadata, Folder, NotificationQueueKey, Notification }, Metadata }, State) ->
    lager:info("Got imap_mailbox_metadata ~p", [Metadata]),
    UID = proplists:get_value(egara_imap_utils:mailbox_uid_header_name(), Metadata, undefined),
    %%lager:info("UID is ~p", [UID]),
    store_folder_notification_with_uid(UID, Folder, Notification, NotificationQueueKey, State#state.storage),
    { noreply, State };
handle_info({ { imap_message_mailbox_metadata, Folder, NotificationQueueKey, Notification }, Metadata }, State) ->
    %%TODO: and if we somehow end up with a folder we can't find, or the uniqueid is not there?
    FolderUID = proplists:get_value(<<"/vendor/cmu/cyrus-imapd/uniqueid">>, Metadata),
    egara_storage:store_folder_uid(State#state.storage, Folder, FolderUID),
    generate_message_event_keys_and_store(State#state.storage, FolderUID, Notification),
    %%lager:info("Message keys ~p, Folder UID to be stored ~p", [Keys, FolderUID]),
    egara_notification_queue:remove(NotificationQueueKey),
    { noreply, State };
handle_info(_Info, State) ->
    { noreply, State }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

%% private API
store_folder_notification_with_uid(undefined, _Folder, Notification, NotificationQueueKey, _Storage) ->
    lager:warning("Could not find UID for notification ~p", [Notification]),
    egara_notification_queue:remove(NotificationQueueKey);
store_folder_notification_with_uid(UID, Folder, Notification, NotificationQueueKey, Storage) ->
    Key = generate_folder_event_key(UID, Notification),
    %%lager:info("Storing folder notification with key ~p", [Key]),
    egara_storage:store_folder_uid(Storage, Folder, UID),
    egara_storage:store_notification(Storage, Key, Notification),
    egara_notification_queue:remove(NotificationQueueKey).

generate_folder_event_key(UID, Notification) ->
    Timestamp = timestamp_from_notification(Notification),
    <<"mailbox::", UID/binary, "::", Timestamp/binary>>.

generate_message_event_keys_and_store(Storage, FolderUID, Notification) ->
    { From, UIDSet } = uidset_from_notification(Notification),
    Timestamp = timestamp_from_notification(Notification),
    Keys = lists:foldl(fun(UID, Acc) -> [<<"message::", FolderUID/binary, "::", UID/binary, "::", Timestamp/binary>> | Acc] end, [], UIDSet),
    %%lager:info("storing an imap_message_event with keys ~p", [Keys])
    store_message_event_with_keys(Storage, Keys, Notification, From, UIDSet).

store_message_event_with_keys(Storage, Keys, Notification, uri, UIDSet) ->
    %% stores the UIDset into the notification to normalize the notification
    NotificationWithUIDSet = [ { <<"uidset">>, UIDSet } | Notification ],
    egara_storage:store_notification(Storage, Keys, NotificationWithUIDSet);
store_message_event_with_keys(Storage, Keys, Notification, _SourceOfUIDSet, _UIDSet) ->
    egara_storage:store_notification(Storage, Keys, Notification).

uidset_from_notification(Notification) ->
    case proplists:get_value(<<"uidset">>, Notification, notfound) of
        notfound -> { uri, [uidset_from_uri(proplists:get_value(<<"uri">>, Notification))] };
        UIDSet -> { notification, binary:split(UIDSet, <<",">>, [trim, global]) }
    end.

uidset_from_uri(URI) when is_binary(URI) ->
    { TagStart, TagEnd } = binary:match(URI, <<";UID=">>),
    UIDStart = TagStart + TagEnd + 1,
    case binary:match(URI, <<";">>, [{ scope, { UIDStart, -1 } }]) of
        nomatch -> binary:part(URI, UIDStart, -1);
        { Semicolon, _ } -> binary:part(URI, UIDStart, Semicolon - UIDStart)
    end.

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

process_as_many_events_as_possible(_State, 0) ->
    egara_notifications_processor:queue_drained(),
    ok;
process_as_many_events_as_possible(State, N) ->
    Status = egara_notification_queue:assign_next(self()),
    %%lager:info("~p is starting to process... ~p", [self(), Key]),
    case notification_assigned(State, Status) of
        again -> process_as_many_events_as_possible(State, N - 1);
        _ -> ok
    end.

notification_assigned(_State, notfound) ->
    %%lager:info("Checking in ~p", [self()]),
    poolboy:checkin(egara_notification_workers, self()),
    egara_notifications_processor:queue_drained(),
    done;
notification_assigned(State, { Key, Notification } ) ->
    EventType = proplists:get_value(<<"event">>, Notification),
    EventCategory = dict:find(EventType, State#state.event_mapping),
    %%lager:info("Type is ~p which maps to ~p", [EventType, EventCategory]),
    Result = process_notification_by_category(State, Notification, EventCategory),
    post_process_event(Key, Result).

post_process_event(Key, { get_mailbox_metadata, Notification }) ->
    Folder = normalized_folder_path_from_notification(Notification),
    start_imap_mailbox_metadata_fetch({ imap_mailbox_metadata, Folder, Key, Notification }, Folder);
post_process_event(Key, { get_message_mailbox_metadata, Notification }) ->
    Folder = normalized_folder_path_from_notification(Notification),
    start_imap_mailbox_metadata_fetch({ imap_message_mailbox_metadata, Folder, Key, Notification }, Folder);
post_process_event(Key, ok) ->
    %%lager:info("Done with ~p", [Key]),
    egara_notification_queue:remove(Key),
    again;
post_process_event(Key, ignoring) ->
    %%lager:info("Ignoring ~p", [Key]),
    egara_notification_queue:remove(Key),
    again;
post_process_event(_, continuing) ->
    again;
post_process_event(Key, _) ->
    egara_notification_queue:release(Key, self()),
    error.

process_notification_by_category(State, Notification, { ok, Type }) when is_record(State, state) ->
    %% this version, with the { ok, _ } tuple is called due to maps:find returning { ok, Value }
    %% it is essentiall a forwarder to other impls below
    NotificationWithUsername = ensure_username(State, Notification, proplists:get_value(<<"user">>, Notification)),
    process_notification_by_category(State#state.storage, NotificationWithUsername, Type);
process_notification_by_category(_, ignore, _) ->
    ignoring;
process_notification_by_category(Storage, Notification, imap_message_event) ->
    case stored_folder_uid_from_notification(Storage, Notification) of
        notfound ->
            { get_message_mailbox_metadata, Notification };
        FolderUID ->
            generate_message_event_keys_and_store(Storage, FolderUID, Notification)
    end;
process_notification_by_category(Storage, Notification, imap_mailbox_event) ->
    case stored_folder_uid_from_notification(Storage, Notification) of
        notfound ->
            { get_mailbox_metadata, Notification };
        UID ->
            Key = generate_folder_event_key(UID, Notification),
            egara_storage:store_notification(Storage, Key, Notification)
    end;
process_notification_by_category(Storage, Notification, imap_session_event) ->
    KeyPrefix = key_prefix_for_session_event(proplists:get_value(<<"event">>, Notification, <<"unknown">>)),
    UserId = userid_from_notification(Notification),
    Timestamp = timestamp_from_notification(Notification),
    Key = <<KeyPrefix/binary, "::", UserId/binary, "::", Timestamp/binary>>,
    %%lager:info("storing an imap_session_event with key ~p", [Key]),
    egara_storage:store_notification(Storage, Key, Notification);
process_notification_by_category(Storage, Notification, imap_quota_event) ->
    UserId = userid_from_notification(Notification),
    Timestamp = timestamp_from_notification(Notification),
    Key = <<"quota::", UserId/binary, "::", Timestamp/binary>>,
    %%lager:info("storing an imap_quota_event with key ~p", [Key]),
    egara_storage:store_notification(Storage, Key, Notification);
process_notification_by_category(_Storage, _Notification, _) ->
    %% in here we have a notification we don't recognize, probably because it was not configured
    %% to be watched for
    ignoring.

key_prefix_for_session_event(<<"Login">>) -> <<"session_login">>;
key_prefix_for_session_event(<<"Logout">>) -> <<"session_logout">>;
key_prefix_for_session_event(_) -> <<"session_event">>.

userid_from_notification(Notification) ->
    case proplists:get_value(<<"user_id">>, Notification, unknown) of
        unknown -> lager:info("Couldn't find the user_id in ~p", [Notification]), proplists:get_value(<<"user">>, Notification, <<"unknown_user">>);
        UserId -> UserId
    end.

timestamp_from_notification(Notification) ->
    case proplists:get_value(<<"timestamp">>, Notification, unknown) of
        unknown -> erlang:list_to_binary(egara_utils:current_timestamp());
        Timestamp -> Timestamp
    end.

ensure_username(_State, Notification, undefined) ->
    Notification;
ensure_username(State, Notification, UserLogin) ->
    case binary:match(UserLogin, State#state.admin_user_prefix) of
        nomatch ->
            FromStorage = egara_storage:fetch_userdata_for_login(State#state.storage, UserLogin),
            %%lager:info("Storage said ... ~p", [FromStorage]),
            add_username_from_storage(State#state.storage, Notification, UserLogin, FromStorage);
        _ -> ignore
    end.

add_username_from_storage(Storage, Notification, UserLogin, notfound) ->
    LDAP = poolboy:checkout(egara_ldap_pool, false, 10),
    RV = query_ldap_for_username(Storage, Notification, UserLogin, LDAP),
    poolboy:checkin(egara_ldap_pool, LDAP),
    RV;
add_username_from_storage(_Storage, Notification, _UserLogin, UserData) ->
    [ { <<"user_id">>, as_binary(proplists:get_value(<<"id">>, UserData, <<"">>)) } | Notification ].

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
    UserIdTuple = { <<"user_id">>, as_binary(proplists:get_value(<<"id">>, UserData, <<"">>)) },
    [ UserIdTuple | Notification ].

normalized_folder_path_from_notification(Notification) ->
    URI = proplists:get_value(<<"uri">>, Notification),
    %%TODO: should we check here if we got an actual URI and not an undefined back?
    %%TODO: use PROPER shared prefix (from IMAP)
    list_to_binary(egara_imap_utils:extract_path_from_uri(none, "/", binary_to_list(URI))).

stored_folder_uid_from_notification(Storage, Notification) ->
    %%TODO caching might help here to avoid hitting storage on every notification
    Folder = normalized_folder_path_from_notification(Notification),
    egara_storage:fetch_folder_uid(Storage, Folder).

start_imap_mailbox_metadata_fetch(Data, Folder) ->
    IMAP = poolboy:checkout(egara_imap_pool, false, 10),
    %%lager:info("fetchng mailbox info over IMAP for ~p with data ~p", [Folder, Data]),
    egara_imap:connect(IMAP), %%TODO, this should be done less often, even though it's nearly a noop here
    egara_imap:get_folder_annotations(IMAP, self(), Data, Folder),
    poolboy:checkin(egara_imap_pool, IMAP).

as_binary(Value) when is_binary(Value) -> Value;
as_binary(Value) when is_list(Value) -> erlang:list_to_binary(Value).
