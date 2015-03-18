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

-record(state, { archival = true, event_mapping, storage, imap, admin_user_prefix, imap_path_delim = "/", imap_shared_prefix = none }).
-record(message_peek_data, { timestamp, folder_path, folder_uid, notification, notification_queue_key, message_uid, uid_set, old_folder_uid, old_message_uid, old_uid_set }).
-record(message_event_getfolderuid_data, { folder, notification_queue_key, notification, event_type }).
-record(message_event_getoldfolderuid_data, { folder, folder_uid, uidset, notification_queue_key, notification, old_folder_path, old_uid_set }).

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    %% EventMapping is a map of event types (e.g. <<"FlagsClear">>) to the type of
    %% event (e.g. imap_mesage_event) for routing the notifications through the handler
    EventMapping = transform_events_config_to_dict(application:get_env(events_to_track)),
    { ok, Storage } = egara_storage:start_link(),
    %% get the admin user, whose events we will ignore
    Archival = application:get_env(egara, archival, false),
    Config = application:get_env(egara, imap, []),
    AdminConnConfig = proplists:get_value(admin_connection, Config, []),
    AdminUser = list_to_binary(proplists:get_value(user, AdminConnConfig, "cyrus-admin")),
    AdminUserPrefix = <<AdminUser/binary, "@">>,
    State = #state{ archival = Archival, event_mapping = EventMapping, storage = Storage, admin_user_prefix = AdminUserPrefix },
    Imap = poolboy:checkout(egara_imap_pool, false, 10),
    egara_imap:connect(Imap),
    egara_imap:get_path_tokens(Imap, self(), get_path_tokens),
    poolboy:checkin(egara_imap_pool, Imap),
    { ok, State }.

handle_call(_Request, _From, State) ->
    { reply, ok, State }.

handle_cast(process_events, State) ->
    Imap = poolboy:checkout(egara_imap_pool, false, 10),
    TempState = State#state{ imap = Imap },
    process_as_many_events_as_possible(TempState, ?BATCH_SIZE),
    poolboy:checkin(egara_imap_pool, Imap),
    { noreply, State };

handle_cast(_Msg, State) ->
    { noreply, State }.

%%TODO: 4 of the handle_info variants are for mailbox metadata fetching; abstract & consolidate?
handle_info({ get_path_tokens, { SharedPrefix, Delim } }, State) ->
    NewState = State#state{ imap_shared_prefix = SharedPrefix, imap_path_delim = Delim },
    { noreply, NewState };
handle_info({ { imap_mailbox_metadata, Folder, NotificationQueueKey, Notification }, Metadata }, State) ->
    lager:info("Got imap_mailbox_metadata ~p", [Metadata]),
    Uid = proplists:get_value(egara_imap_utils:header_name(mailbox_uid), Metadata, undefined),
    %%lager:info("Uid is ~p", [Uid]),
    NotificationWithMetadata = [ { <<"metadata">>, Metadata } | Notification ],
    Result = store_folder_notification_with_uid(Uid, Folder, NotificationWithMetadata, State#state.storage),
    notification_processing_result(NotificationQueueKey, Result, State),
    { noreply, State };
handle_info({ #message_event_getfolderuid_data{ folder = Folder, notification_queue_key = NotificationQueueKey,
                                                 notification = Notification, event_type = EventType }, Metadata }, State) ->
    %%TODO: and if we somehow end up with a folder we can't find, or the uniqueid is not there?
    FolderUid = proplists:get_value(<<"/vendor/cmu/cyrus-imapd/uniqueid">>, Metadata),
    egara_storage:store_folder_uid(State#state.storage, Folder, FolderUid),
    Result = store_message_event(State, FolderUid, Notification, EventType, NotificationQueueKey),
    notification_processing_result(NotificationQueueKey, Result, State),
    %%lager:info("Message keys ~p, Folder Uid to be stored ~p", [Keys, FolderUid]),
    { noreply, State };
handle_info({ #message_event_getoldfolderuid_data{ folder = FolderPath, folder_uid = FolderUid, uidset = UidSet,
                                                   notification_queue_key = NotificationQueueKey,
                                                   notification = Notification,
                                                   old_folder_path = OldFolderPath, old_uid_set = OldUidSet}, Metadata }, State) ->
    OldFolderUid = proplists:get_value(<<"/vendor/cmu/cyrus-imapd/uniqueid">>, Metadata),
    egara_storage:store_folder_uid(State#state.storage, OldFolderPath, OldFolderUid),
    Timestamp = timestamp_from_notification(Notification),
    start_message_peek(State#state.imap, Timestamp, Notification, FolderPath, FolderUid, UidSet, OldFolderUid, OldUidSet, NotificationQueueKey),
    { noreply, State };
handle_info({ MessagePeekData, Result }, State) when is_record(MessagePeekData, message_peek_data) ->
    message_peek_received(State, MessagePeekData, Result),
    { noreply, State };
handle_info(_Info, State) ->
    { noreply, State }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

%% private API
message_peek_received(State, #message_peek_data{ folder_path = FolderPath, folder_uid = FolderUid, notification = Notification } = MessagePeekData, mailboxnotfound) ->
    lager:error("Mailbox ~p (~p) could not be found for message notification { ~p }", [FolderPath, FolderUid, Notification]),
    message_peek_iteration(MessagePeekData, Notification, State),
    { noreply, State };
message_peek_received(State, #message_peek_data{ folder_path = FolderPath, message_uid = MessageUid, notification = Notification } = MessagePeekData, error) ->
    lager:error("Message ~p (~p) could not be found for message notification { ~p }", [MessageUid, FolderPath, Notification]),
    message_peek_iteration(MessagePeekData, Notification, State),
    { noreply, State };
message_peek_received(State, #message_peek_data{ notification = Notification } = MessagePeekData, Data) ->
    PeekedNotification = lists:foldl(fun(Atom, Notif) -> add_entry_to_notification(Atom, Data, Notif) end, Notification, [flags, headers, message]),
    message_peek_iteration(MessagePeekData, PeekedNotification, State),
    { noreply, State }.

message_peek_iteration(#message_peek_data{ timestamp = Timestamp, folder_path = FolderPath, folder_uid = FolderUid, notification = Notification,
                                           notification_queue_key = NotificationQueueKey, message_uid = MessageUid, uid_set = UidSet,
                                           old_folder_uid = OldFolderUid, old_message_uid = OldMessageUid, old_uid_set = OldUidSet },
                         StorableNotification, #state { imap = Imap, storage = Storage } = State) ->
    GroupwareUid = proplists:get_value(<<"groupware_uid">>, StorableNotification),
    egara_history_entry:store(Storage, Timestamp, GroupwareUid, FolderUid, MessageUid, OldFolderUid, OldMessageUid),
    Key = generate_message_event_key(FolderUid, MessageUid, Timestamp),
    egara_storage:store_notification(Storage, Key, StorableNotification),
    case start_message_peek(Imap, Timestamp, Notification,
                            FolderPath, FolderUid, UidSet,
                            OldFolderUid, OldUidSet,
                            NotificationQueueKey) of
        done ->
            notification_processing_result(NotificationQueueKey, ok, State);
        _ -> ok
    end.

add_entry_to_notification(undefined, _Data, Notification) ->
    Notification;
add_entry_to_notification(headers, Data, Notification) ->
    Headers = proplists:get_value(headers, Data),
    NotificationWithGroupwareUid =
    case groupware_uid_from_headers(Headers) of
        undefined -> Notification;
        GroupwareUid -> [{ <<"groupware_uid">>, GroupwareUid } | Notification]
    end,
    add_entry_to_notification(<<"headers">>, Headers, NotificationWithGroupwareUid);
add_entry_to_notification(Atom, Data, Notification) when is_atom(Atom) ->
    add_entry_to_notification(atom_to_binary(Atom, utf8), proplists:get_value(Atom, Data), Notification);
add_entry_to_notification(Key, Value, Notification) when is_binary(Key) ->
    %%TODO: check if already there with proplists:get_value(Key, Data)
    [{ Key, Value } | Notification].

store_folder_notification_with_uid(undefined, _Folder, Notification, _Storage) ->
    lager:warning("Could not find Uid for notification ~p", [Notification]),
    ok;
store_folder_notification_with_uid(Uid, Folder, Notification, Storage) ->
    Key = generate_folder_event_key(Uid, Notification),
    %%lager:info("Storing folder notification with key ~p", [Key]),
    egara_storage:store_folder_uid(Storage, Folder, Uid),
    egara_storage:store_notification(Storage, Key, Notification).

generate_folder_event_key(Uid, Notification) ->
    Timestamp = timestamp_from_notification(Notification),
    <<"mailbox::", Uid/binary, "::", Timestamp/binary>>.

store_message_event(State, FolderUid, Notification, EventType, NotificationQueueKey)
    when EventType =:= <<"MessageNew">> orelse
         EventType =:= <<"MessageAppend">> orelse
         EventType =:= <<"MessageCopy">> orelse
         EventType =:= <<"MessageMove">> ->
    %% we need to fetch message content here and begin generating history events while doing so
    { NotificationWithUidset, UidSet } = uidset_from_notification(Notification),
    FolderPath = normalized_folder_path_from_notification(Notification, State),
    OldFolderUri = proplists:get_value(<<"oldMailboxID">>, Notification),
    case old_location(State, Notification, OldFolderUri) of
        { undefined, OldUidSet } ->
            %% we don't have the UID for the older folder, let's go fetch it
            OldFolderPath = egara_imap_utils:extract_path_from_uri(State#state.imap_shared_prefix, State#state.imap_path_delim, OldFolderUri),
            Data = #message_event_getoldfolderuid_data{ folder = FolderPath, folder_uid = FolderUid,
                                                        uidset = UidSet,
                                                        notification_queue_key = NotificationQueueKey,
                                                        notification = NotificationWithUidset,
                                                        old_folder_path = OldFolderPath, old_uid_set = OldUidSet },
            start_imap_mailbox_metadata_fetch(Data, OldFolderPath, State);
        { OldFolderUid, OldUidSet } ->
            Timestamp = timestamp_from_notification(Notification),
            start_message_peek(State#state.imap, Timestamp, Notification, FolderPath, FolderUid, UidSet, OldFolderUid, OldUidSet, NotificationQueueKey)
    end,
    continuing;
store_message_event(State, FolderUid, Notification, _Type, _NotificationQueueKey) ->
    { NotificationWithUidset, UidSet } = uidset_from_notification(Notification),
    Timestamp = timestamp_from_notification(Notification),
    %%lager:info("storing an imap_message_event with keys ~p", [Keys]),
    store_next_message_event(State, NotificationWithUidset, Timestamp, FolderUid, egara_imap_uidset:next_uid(UidSet)).

store_next_message_event(_State, _Notification, _Timestamp, _FolderUid, { none, _UidSet }) ->
    ok;
store_next_message_event(#state{ storage = Storage } = State, Notification, Timestamp, FolderUid, { MessageUid, UidSet } ) ->
    Key = generate_message_event_key(FolderUid, MessageUid, Timestamp),
    egara_storage:store_notification(Storage, Key, Notification),
    store_message_event(State, Notification, Timestamp, FolderUid, egara_imap_uidset:next_uid(UidSet)).

generate_message_event_key(FolderUid, MessageUid, Timestamp) when is_integer(MessageUid) ->
    UidBin = integer_to_binary(MessageUid),
    <<"message::", FolderUid/binary, "::", UidBin/binary, "::", Timestamp/binary>>.


groupware_uid_from_headers(undefined) ->
    undefined;
groupware_uid_from_headers(Headers) ->
    groupware_uid_from_headers(Headers, proplists:get_value(<<"X-Kolab-Type">>, Headers)).

groupware_uid_from_headers(_Headers, undefined) ->
    undefined;
groupware_uid_from_headers(Headers, _) ->
    proplists:get_value(<<"Subject">>, Headers).

old_location(_State, _Notification, undefined) ->
    OldFolderUid = <<"">>,
    OldUidSet = egara_imap_uidset:parse(<<"">>),
    { OldFolderUid, OldUidSet };
old_location(State, Notification, OldFolderUri) ->
    OldUidSet = egara_imap_uidset:parse(proplists:get_value(<<"vnd.cmu.oldUidset">>, Notification)),
    OldFolderPath = egara_imap_utils:extract_path_from_uri(State#state.imap_shared_prefix, State#state.imap_path_delim, OldFolderUri),
    OldFolderUid = egara_storage:fetch_folder_uid(State#state.storage, OldFolderPath),
    { OldFolderUid, OldUidSet }.

uidset_from_notification(Notification) ->
    uidset_from_notification(Notification, proplists:get_value(<<"uidset">>, Notification)).
uidset_from_notification(Notification, undefined) ->
    UidSetString = egara_imap_utils:extract_uidset_from_uri(proplists:get_value(<<"uri">>, Notification)),
    { [{ <<"uidset">>, UidSetString }|Notification], egara_imap_uidset:parse(UidSetString) };
uidset_from_notification(Notification, UidSetString) ->
    { Notification, egara_imap_uidset:parse(UidSetString) }.

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
notification_assigned(State, { QueueKey, Notification } ) ->
    EventType = proplists:get_value(<<"event">>, Notification),
    EventCategory = dict:find(EventType, State#state.event_mapping),
    %%lager:info("Type is ~p which maps to ~p", [EventType, EventCategory]),
    Result = process_notification_by_category(State, Notification, EventCategory, EventType, QueueKey),
    notification_processing_result(QueueKey, Result, State).

notification_processing_result(QueueKey, ok, _State) ->
    %%lager:info("Done with ~p", [QueueKey]),
    egara_notification_queue:remove(QueueKey),
    again;
notification_processing_result(QueueKey, unrecoverable_error, _State) ->
    lager:error("Event ~p could not be processed, dropping on floor", [QueueKey]),
    egara_notification_queue:remove(QueueKey),
    again;
notification_processing_result(QueueKey, ignore, _State) ->
    %%lager:info("Ignoring ~p", [QueueKey]),
    egara_notification_queue:remove(QueueKey),
    again;
notification_processing_result(_, continuing, _State) ->
    again;
notification_processing_result(QueueKey, _, _State) ->
    egara_notification_queue:release(QueueKey, self()),
    error.

process_notification_by_category(State, Notification, { ok, Category }, Type, QueueKey) when is_record(State, state) ->
    %% this version, with the { ok, _ } tuple is called due to maps:find returning { ok, Value }
    %% it is essentially a forwarder to other impls below
    case ensure_username(State, Notification, proplists:get_value(<<"user">>, Notification)) of
        ignore -> ignore;
        NotificationWithUsername ->
            process_notification_by_category(State, NotificationWithUsername, Category, Type, QueueKey)
    end;
process_notification_by_category(State, Notification, imap_message_event, Type, QueueKey) ->
    case stored_folder_uid_from_notification(State, Notification) of
        notfound ->
            Folder = normalized_folder_path_from_notification(Notification, State),
            Data = #message_event_getfolderuid_data{ folder = Folder, notification_queue_key = QueueKey, notification = Notification, event_type = Type },
            start_imap_mailbox_metadata_fetch(Data, Folder, State),
            continuing;
        FolderUid ->
            store_message_event(State, FolderUid, Notification, Type, QueueKey)
    end;
process_notification_by_category(State, Notification, imap_mailbox_event, _Type, QueueKey) ->
    case stored_folder_uid_from_notification(State, Notification) of
        notfound ->
            Folder = normalized_folder_path_from_notification(Notification, State),
            start_imap_mailbox_metadata_fetch({ imap_mailbox_metadata, Folder, QueueKey, Notification }, Folder, State),
            continuing;
        Uid ->
            Key = generate_folder_event_key(Uid, Notification),
            egara_storage:store_notification(State#state.storage, Key, Notification)
    end;
process_notification_by_category(State, Notification, imap_session_event, Type, _QueueKey) ->
    KeyPrefix = key_prefix_for_session_event(Type),
    UserId = userid_from_notification(Notification),
    Timestamp = timestamp_from_notification(Notification),
    Key = <<KeyPrefix/binary, "::", UserId/binary, "::", Timestamp/binary>>,
    %%lager:info("storing an imap_session_event with key ~p", [Key]),
    egara_storage:store_notification(State#state.storage, Key, Notification);
process_notification_by_category(State, Notification, imap_quota_event, _Type, _QueueKey) ->
    UserId = userid_from_notification(Notification),
    Timestamp = timestamp_from_notification(Notification),
    Key = <<"quota::", UserId/binary, "::", Timestamp/binary>>,
    %%lager:info("storing an imap_quota_event with key ~p", [Key]),
    egara_storage:store_notification(State#state.storage, Key, Notification);
process_notification_by_category(_State, _Notification, _CategoryFail, _Type, _QueueKey) ->
    %% in here we have a notification that is not in our configuration
    ignore.

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
        unknown -> egara_utils:current_timestamp();
        Timestamp -> egara_utils:normalize_timestamp(Timestamp)
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

normalized_folder_path_from_notification(Notification, State) ->
    Uri = proplists:get_value(<<"uri">>, Notification),
    egara_imap_utils:extract_path_from_uri(State#state.imap_shared_prefix, State#state.imap_path_delim, Uri).

stored_folder_uid_from_notification(State, Notification) ->
    %%TODO caching might help here to avoid hitting storage on every notification
    Folder = normalized_folder_path_from_notification(Notification, State),
    egara_storage:fetch_folder_uid(State#state.storage, Folder).

start_imap_mailbox_metadata_fetch(Data, Folder, #state{ imap = Imap }) ->
    %%lager:info("fetchng mailbox info over IMAP for ~p with data ~p", [Folder, Data]),
    egara_imap:connect(Imap), %%TODO, this should be done less often, even though it's nearly a noop here
    egara_imap:get_folder_annotations(Imap, self(), Data, Folder).

start_message_peek(_Imap, _Timestamp, _Notification, _FolderPath, _FolderUid, { none, _ }, _OldFolderUid, { _, _OldUidSet }, _NotificationQueueKey) ->
    done;
start_message_peek(Imap, Timestamp, Notification, FolderPath, FolderUid, { MessageUid, UidSet }, OldFolderUid, { OldMessageUid, OldUidSet }, NotificationQueueKey) ->
    %%lager:info("fetching message headers/flags/body over IMAP for message ~p in ~p (UID: ~p)", [MessageUid, FolderPath, FolderUid]),
    Data = #message_peek_data{ timestamp = Timestamp, folder_path = FolderPath, folder_uid = FolderUid,
                               notification = Notification, notification_queue_key = NotificationQueueKey,
                               message_uid = MessageUid, uid_set = UidSet,
                               old_folder_uid = OldFolderUid, old_message_uid = OldMessageUid, old_uid_set = OldUidSet},
    egara_imap:connect(Imap),
    egara_imap:get_message_headers_and_body(Imap, self(), Data, FolderPath, MessageUid),
    continue;
start_message_peek(Imap, Timestamp, Notification, FolderPath, FolderUid, UidSet, OldFolderUid, OldUidSet, NotificationQueueKey) ->
    start_message_peek(Imap, Timestamp, Notification,
                       FolderPath, FolderUid, egara_imap_uidset:next_uid(UidSet),
                       OldFolderUid, egara_imap_uidset:next_uid(OldUidSet), NotificationQueueKey).

as_binary(Value) when is_binary(Value) -> Value;
as_binary(Value) when is_list(Value) -> erlang:list_to_binary(Value).
