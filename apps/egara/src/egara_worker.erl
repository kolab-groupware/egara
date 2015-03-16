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

-record(state, { event_mapping, storage, imap, admin_user_prefix, imap_path_delim = "/", imap_shared_prefix = none }).

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
    Uid = proplists:get_value(egara_imap_utils:mailbox_uid_header_name(), Metadata, undefined),
    %%lager:info("Uid is ~p", [Uid]),
    NotificationWithMetadata = [ { <<"metadata">>, Metadata } | Notification ],
    Result = store_folder_notification_with_uid(Uid, Folder, NotificationWithMetadata, State#state.storage),
    post_process_event(NotificationQueueKey, Result, State),
    { noreply, State };
handle_info({ { imap_message_mailbox_metadata, Folder, NotificationQueueKey, Notification }, Metadata }, State) ->
    %%TODO: and if we somehow end up with a folder we can't find, or the uniqueid is not there?
    FolderUid = proplists:get_value(<<"/vendor/cmu/cyrus-imapd/uniqueid">>, Metadata),
    egara_storage:store_folder_uid(State#state.storage, Folder, FolderUid),
    EventType = proplists:get_value(<<"event">>, Notification),
    Result = generate_message_event_keys_and_store(State, FolderUid, Notification, EventType),
    post_process_event(NotificationQueueKey, Result, State),
    %%lager:info("Message keys ~p, Folder Uid to be stored ~p", [Keys, FolderUid]),
    { noreply, State };
handle_info({ { historyentry_old_mailbox_metadata, Timestamp, NewFolderUid, NewUidSet, OldFolderPath, OldUidSet }, Metadata }, State) ->
    OldFolderUid = proplists:get_value(<<"/vendor/cmu/cyrus-imapd/uniqueid">>, Metadata),
    egara_storage:store_folder_uid(State#state.storage, OldFolderPath, OldFolderUid),
    create_message_history_entry(State, Timestamp, NewFolderUid, NewUidSet, OldFolderUid, OldUidSet),
    { noreply, State };
handle_info({ { message_peek, FolderUid, NotificationQueueKey, Notification }, mailboxnotfound }, State) ->
    Folder = normalized_folder_path_from_notification(Notification, State),
    lager:error("Mailbox ~p (~p) could not be found for message notification { ~s }", [Folder, FolderUid, Notification]),
    post_process_event(NotificationQueueKey, unrecoverable_error, State),
    { noreply, State };
handle_info({ { message_peek, FolderUid, NotificationQueueKey, Notification }, Data }, State) ->
    PeekedNotification = lists:foldl(fun(Atom, Acc) -> add_entry_to_notification(Acc, Data, Atom) end,
                                     Notification, [flags, headers, body]),
    create_message_history_entry(State, FolderUid, PeekedNotification),
    Result = generate_message_event_keys_and_store(State, FolderUid, PeekedNotification),
    post_process_event(NotificationQueueKey, Result, State),
    { noreply, State };
handle_info(_Info, State) ->
    { noreply, State }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

%% private API
add_entry_to_notification(Notification, Data, Atom) when is_atom(Atom) ->
    add_entry_to_notification(Notification, atom_to_binary(Atom, utf8), proplists:get_value(Atom, Data));
add_entry_to_notification(Notification, Key, undefined) when is_binary(Key) ->
    Notification;
add_entry_to_notification(Notification, Key, Value) when is_binary(Key) ->
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

generate_message_event_keys_and_store(_State, FolderUid, Notification, <<"MessageNew">>) ->
    %%create_message_history_entry(State, FolderUid, Notification),
    { message_peek, FolderUid, Notification };
generate_message_event_keys_and_store(State, FolderUid, Notification, <<"MessageAppend">>) ->
    create_message_history_entry(State, FolderUid, Notification),
    { message_peek, FolderUid, Notification };
generate_message_event_keys_and_store(_State, FolderUid, Notification, <<"MessageCopy">>) ->
    { message_peek, FolderUid, Notification };
generate_message_event_keys_and_store(State, FolderUid, Notification, <<"MessageMove">>) ->
    create_message_history_entry(State, FolderUid, Notification),
    generate_message_event_keys_and_store(State, FolderUid, Notification);
generate_message_event_keys_and_store(State, FolderUid, Notification, _Type) ->
    generate_message_event_keys_and_store(State, FolderUid, Notification).

generate_message_event_key(_FolderUid, _Timestamp, Acc, { none, _ }) ->
    Acc;
generate_message_event_key(FolderUid, Timestamp, Acc, { Uid, UidSet }) ->
    UidBin = integer_to_binary(Uid),
    Key = <<"message::", FolderUid/binary, "::", UidBin/binary, "::", Timestamp/binary>>,
    generate_message_event_key(FolderUid, Timestamp, [Key|Acc], egara_imap_uidset:next_uid(UidSet)).
generate_message_event_keys(FolderUid, Timestamp, UidSetString) ->
    UidSet = egara_imap_uidset:parse(UidSetString),
    generate_message_event_key(FolderUid, Timestamp, [], egara_imap_uidset:next_uid(UidSet)).

generate_message_event_keys_and_store(#state{ storage = Storage }, FolderUid, Notification) ->
    { UidSetFrom, UidSet } = uidset_from_notification(Notification),
    Timestamp = timestamp_from_notification(Notification),
    Keys = generate_message_event_keys(FolderUid, Timestamp, UidSet),
    lager:info("storing an imap_message_event with keys ~p", [Keys]),
    store_message_event_with_keys(Storage, Keys, Notification, UidSetFrom, UidSet).

create_message_history_entry(State, NewFolderUid, Notification) ->
    OldUri = proplists:get_value(<<"oldMailboxID">>, Notification),
    create_message_history_entry(State, NewFolderUid, Notification, OldUri).

create_message_history_entry(_State, _NewFolderUid, _Notification, undefined) ->
    %% no old uri, so this isn't a moment in history.
    %% UidSet = proplists:get_value(<<"uidset">>, Notification, <<"">>),
    %% OldFolder = <<"">>,
    %% OldUid = <<"">>,
    %% create_message_history_entry(State, FolderUid, UidSet, Notification, OldFolder, OldUidSet);
    ok;
create_message_history_entry(State, NewFolderUid, Notification, OldFolderUri) ->
    Timestamp = timestamp_from_notification(Notification),
    NewUidSet = proplists:get_value(<<"uidset">>, Notification, <<"">>),
    OldUidSet = proplists:get_value(<<"vnd.cmu.oldUidset">>, Notification),
    OldFolderPath = egara_imap_utils:extract_path_from_uri(State#state.imap_shared_prefix, State#state.imap_path_delim, OldFolderUri),
    OldFolderUid = egara_storage:fetch_folder_uid(State#state.storage, OldFolderPath),
    store_message_history_entry_with_oldFolderUid(State, Timestamp, NewFolderUid, NewUidSet, OldFolderPath, OldFolderUid, OldUidSet).

store_message_history_entry_with_oldFolderUid(State, Timestamp, NewFolderUid, NewUidSet, OldFolderPath, notfound, OldUidSet) ->
    %% have to get the old folder uid
    start_imap_mailbox_metadata_fetch({ historyentry_old_mailbox_metadata, Timestamp, NewFolderUid, NewUidSet, OldFolderPath, OldUidSet }, OldFolderPath, State);
store_message_history_entry_with_oldFolderUid(State, Timestamp, NewFolderUid, NewUidSet, _OldFolderPath, OldFolderUid, OldUidSet) ->
    create_message_history_entry(State, Timestamp, NewFolderUid, NewUidSet, OldFolderUid, OldUidSet).

create_message_history_entry(State, Timestamp, NewFolderUid, NewUidSet, OldFolderUid, OldUidSet) ->
    Key = <<NewUidSet/binary, "::", NewFolderUid/binary, "::", Timestamp>>,
    Value = <<OldUidSet/binary, "::", OldFolderUid/binary>>,
    egara_storage:store_message_history_entry(State#state.storage, Key, Value).

store_message_event_with_keys(Storage, Keys, Notification, uri, UidSet) ->
    %% stores the Uidset into the notification to normalize the notification
    NotificationWithUidSet = [ { <<"uidset">>, UidSet } | Notification ],
    egara_storage:store_notification(Storage, Keys, NotificationWithUidSet);
store_message_event_with_keys(Storage, Keys, Notification, _SourceOfUidSet, _UidSet) ->
    egara_storage:store_notification(Storage, Keys, Notification).

uidset_from_notification(Notification) ->
    case proplists:get_value(<<"uidset">>, Notification, notfound) of
        notfound -> { uri, [egara_imap_utils:extract_uidset_from_uri(proplists:get_value(<<"uri">>, Notification))] };
        UidSet -> { notification, binary:split(UidSet, <<",">>, [trim, global]) }
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
    Result = process_notification_by_category(State, Notification, EventCategory, EventType),
    post_process_event(Key, Result, State).

post_process_event(Key, { get_mailbox_metadata, Notification }, State) ->
    Folder = normalized_folder_path_from_notification(Notification, State),
    start_imap_mailbox_metadata_fetch({ imap_mailbox_metadata, Folder, Key, Notification }, Folder, State);
post_process_event(Key, { get_message_mailbox_metadata, Notification }, State) ->
    Folder = normalized_folder_path_from_notification(Notification, State),
    start_imap_mailbox_metadata_fetch({ imap_message_mailbox_metadata, Folder, Key, Notification }, Folder, State);
post_process_event(Key, { message_peek, FolderUid, Notification }, State) ->
    { _, [Message | _] } = uidset_from_notification(Notification),
    Folder = normalized_folder_path_from_notification(Notification, State),
    start_message_peek({ message_peek, FolderUid, Key, Notification }, Folder, Message, State);
post_process_event(Key, ok, _State) ->
    %%lager:info("Done with ~p", [Key]),
    egara_notification_queue:remove(Key),
    again;
post_process_event(Key, unrecoverable_error, _State) ->
    lager:error("Event ~p could not be processed, dropping on floor", [Key]),
    egara_notification_queue:remove(Key),
    again;
post_process_event(Key, ignoring, _State) ->
    %%lager:info("Ignoring ~p", [Key]),
    egara_notification_queue:remove(Key),
    again;
post_process_event(_, continuing, _State) ->
    again;
post_process_event(Key, _, _State) ->
    egara_notification_queue:release(Key, self()),
    error.

process_notification_by_category(State, Notification, { ok, Category }, Type) when is_record(State, state) ->
    %% this version, with the { ok, _ } tuple is called due to maps:find returning { ok, Value }
    %% it is essentially a forwarder to other impls below
    NotificationWithUsername = ensure_username(State, Notification, proplists:get_value(<<"user">>, Notification)),
    process_notification_by_category(State, NotificationWithUsername, Category, Type);
process_notification_by_category(State, Notification, imap_message_event, _Type) ->
    case stored_folder_uid_from_notification(State, Notification) of
        notfound ->
            { get_message_mailbox_metadata, Notification };
        FolderUid ->
            EventType = proplists:get_value(<<"event">>, Notification),
            generate_message_event_keys_and_store(State, FolderUid, Notification, EventType)
    end;
process_notification_by_category(State, Notification, imap_mailbox_event, _Type) ->
    case stored_folder_uid_from_notification(State, Notification) of
        notfound ->
            { get_mailbox_metadata, Notification };
        Uid ->
            Key = generate_folder_event_key(Uid, Notification),
            egara_storage:store_notification(State#state.storage, Key, Notification)
    end;
process_notification_by_category(State, Notification, imap_session_event, Type) ->
    KeyPrefix = key_prefix_for_session_event(Type),
    UserId = userid_from_notification(Notification),
    Timestamp = timestamp_from_notification(Notification),
    Key = <<KeyPrefix/binary, "::", UserId/binary, "::", Timestamp/binary>>,
    %%lager:info("storing an imap_session_event with key ~p", [Key]),
    egara_storage:store_notification(State#state.storage, Key, Notification);
process_notification_by_category(State, Notification, imap_quota_event, _Type) ->
    UserId = userid_from_notification(Notification),
    Timestamp = timestamp_from_notification(Notification),
    Key = <<"quota::", UserId/binary, "::", Timestamp/binary>>,
    %%lager:info("storing an imap_quota_event with key ~p", [Key]),
    egara_storage:store_notification(State#state.storage, Key, Notification);
process_notification_by_category(_State, _Notification, _CategoryFail, _Type) ->
    %% in here we have a notification that is not in our configuration
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

start_message_peek(Data, Folder, Message, #state{ imap = Imap }) ->
    %%lager:info("fetching message headers/flags/body over IMAP for ~p ~p with data ~p", [Folder, Message, Data]),
    egara_imap:connect(Imap), %%TODO, this should be done less often, even though it's nearly a noop here
    egara_imap:get_message_headers_and_body(Imap, self(), Data, Folder, Message).

as_binary(Value) when is_binary(Value) -> Value;
as_binary(Value) when is_list(Value) -> erlang:list_to_binary(Value).
