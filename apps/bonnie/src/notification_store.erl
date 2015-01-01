-module(notification_store).
-export([ install/1, start/0,
          notification/1, next_unnasigned/0,
          add/2, add/3, remove/1,
          assign/2, assign_next/1,
          release/1, release/2, release_orphaned/0 ]).
-include_lib("stdlib/include/qlc.hrl").
-record(egara_incoming_notification, { id, claimed = 0, term }).

install(Nodes) ->
    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    try mnesia:create_table(egara_incoming_notification,
                            [{ attributes, record_info(fields, egara_incoming_notification) },
                             { type, ordered_set },
                             { disc_copies, Nodes }]) of
        _ -> ok
    catch
        error:_ -> ok
    end.

start() ->
    mnesia:wait_for_tables([egara_incoming_notification], 5000).

add(Key, Term) ->
    F = fun() ->
                Rec = #egara_incoming_notification{ id = Key, term = Term },
                mnesia:write(Rec)
        end,
    mnesia:activity(transaction, F).

add(Key, Term, PID) when is_pid(PID) ->
    F = fun() ->
                Rec = #egara_incoming_notification{ id = Key, term = Term, claimed = PID },
                mnesia:write(Rec)
        end,
    mnesia:activity(transaction, F).

remove(Key) ->
    F = fun() -> try mnesia:delete({ egara_incoming_notification, Key }) of
                     _ -> ok
                 catch
                     error:Error -> {error, caught, Error}
                 end
                 end,
    mnesia:activity(transaction, F).

notification(Key) ->
    F = fun() ->
                case mnesia:read(egara_incoming_notification, Key) of
                    [Record] -> Record;
                    [] -> #egara_incoming_notification{ id = 0 }
                end
        end,
    mnesia:activity(transaction, F).

assign(#egara_incoming_notification{ id = Key }, PID) ->
    assign(Key, PID);

assign(Key, PID) ->
    Pattern = #egara_incoming_notification{ _ = '_', id = Key, claimed = 0 },
    F = fun() ->
                %% check if claimed is set and if so if the process is still running
                case mnesia:match_object(Pattern) of
                    [#egara_incoming_notification{ term = Term }] -> mnesia:write(#egara_incoming_notification{ id = Key, term = Term, claimed = PID }), true;
                    [] -> { error, notfound}
                end
        end,
    mnesia:activity(transaction, F).

release(Key, PID) ->
    Pattern = #egara_incoming_notification{ _ = '_', claimed = PID, id = Key },
    F = fun() ->
                %% check if claimed is set and if so if the process is still running
                case mnesia:match_object(Pattern) of
                    [#egara_incoming_notification{ term = Term }] -> mnesia:write(#egara_incoming_notification{ id = Key, term = Term, claimed = 0 });
                    [] -> { error, notfound}
                end
        end,
    mnesia:activity(transaction, F).

release(Key) ->
    F = fun() ->
                case mnesia:read(egara_incoming_notification, Key) of
                    [#egara_incoming_notification{ term = Term }] -> mnesia:write(#egara_incoming_notification{ id = Key, term = Term, claimed = 0 }), true;
                    [] -> false
                end
        end,
    mnesia:activity(transaction, F).

release_orphaned() ->
    F = fun() ->
                QH = qlc:q([ Key || #egara_incoming_notification{ id = Key, claimed = PID } <- mnesia:table(egara_incoming_notification),
                             is_pid(PID), process_info(PID) =:= undefined]),
                qlc:fold(fun(Key, N) -> release(Key), N + 1 end, 0, QH)
        end,
    mnesia:activity(transaction, F).

orphan_assigner([Record | Tail], PID) ->
    Result = assign(Record#egara_incoming_notification.id, PID),
    if Result =:= true -> true;
       true -> orphan_assigner(Tail, PID)
    end;
orphan_assigner([], _) -> false.

assign_next(PID) when is_pid(PID) ->
    Pattern = #egara_incoming_notification{ _ = '_', claimed = 0 },
    F = fun() ->
                case mnesia:match_object(Pattern) of
                    [] -> false;
                    Unnasigned when is_list(Unnasigned) -> orphan_assigner(Unnasigned, PID);
                    _ -> false
                end
        end,
    mnesia:activity(transaction, F).

next_unnasigned() ->
    Pattern = #egara_incoming_notification{ _ = '_', claimed = 0 },
    F = fun() ->
                case mnesia:match_object(Pattern) of
                    [] -> #egara_incoming_notification{ id = 0 };
                    Record when is_list(Record) -> hd(Record);
                    _ -> #egara_incoming_notification{ id = 0 }
                end
        end,
    mnesia:activity(transaction, F).

