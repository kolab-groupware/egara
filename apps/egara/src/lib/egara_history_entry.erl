-module(egara_history_entry).
-export([store/7]).

store(_Storage, _Timestamp, _GroupwareUid, _NewFolderUid, none, _OldFolderUid, _OldMessageUid) ->
    ok;
store(Storage, Timestamp, GroupwareUid, NewFolderUid, NewMessageUid, OldFolderUid, none) ->
    NewMessageUidBin = integer_to_binary(NewMessageUid),
    Key = generate_history_key(Storage, Timestamp, GroupwareUid, NewFolderUid, NewMessageUidBin),
    ValueJson = <<"{ \"groupware_uid\": ", GroupwareUid/binary, " \"history\": {} }">>,
    egara_storage:store_message_history_entry(Storage, Key, ValueJson);
store(Storage, Timestamp, GroupwareUid, NewFolderUid, NewMessageUid, OldFolderUid, OldMessageUid) ->
    NewMessageUidBin = integer_to_binary(NewMessageUid),
    OldMessageUidBin = integer_to_binary(OldMessageUid),
    Key = generate_history_key(Storage, Timestamp, GroupwareUid, NewFolderUid, NewMessageUidBin),
    ValueTerm = [{<<"groupware_uid">>, GroupwareUid }, { <<"history">>, [{ <<"imap">>, [{ <<"previous_id">>, OldMessageUidBin }, { <<"previous_folder">>, OldFolderUid }] }] }],
    ValueJson = jsx:encode(ValueTerm),
    egara_storage:store_message_history_entry(Storage, Key, ValueJson).

%% PRIVATE
generate_history_key(_, Timestamp, undefined, FolderUid, MessageUid) ->
    <<"message::", FolderUid/binary, "::", MessageUid/binary, "::", Timestamp/binary, "::">>;
generate_history_key(_, Timestamp, GroupwareUid, FolderUid, MessageUid) ->
    <<"message::", FolderUid/binary, "::", MessageUid/binary, "::", Timestamp/binary, "::", GroupwareUid/binary>>.


