-module(egara_history_entry).
-export([store/6]).

store(_Storage, _Timestamp, _NewFolderUid, none, _OldFolderUid, _OldMessageUid) ->
    ok;
store(Storage, Timestamp, NewFolderUid, NewMessageUid, OldFolderUid, none) ->
    NewMessageUidBin = integer_to_binary(NewMessageUid),
    Key = generate_history_key(NewFolderUid, NewMessageUidBin, Timestamp),
    ValueJson = <<"{ \"history\": {} }">>,
    egara_storage:store_message_history_entry(Storage, Key, ValueJson);
store(Storage, Timestamp, NewFolderUid, NewMessageUid, OldFolderUid, OldMessageUid) ->
    NewMessageUidBin = integer_to_binary(NewMessageUid),
    OldMessageUidBin = integer_to_binary(OldMessageUid),
    Key = generate_history_key(NewFolderUid, NewMessageUidBin, Timestamp),
    ValueTerm = [{ <<"history">>, [{ <<"imap">>, [{ <<"previous_id">>, OldMessageUidBin }, { <<"previous_folder">>, OldFolderUid }] }] }],
    ValueJson = jsx:encode(ValueTerm),
    egara_storage:store_message_history_entry(Storage, Key, ValueJson).

%% PRIVATE
generate_history_key(FolderUid, MessageUid, Timestamp) -> <<"message::", FolderUid/binary, "::", MessageUid/binary, "::", Timestamp/binary>>.


