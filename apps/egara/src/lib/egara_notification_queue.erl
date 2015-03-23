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

-module(egara_notification_queue).
-export([ install/1, start/0,
          notification/1, next_unassigned/0, process_next_unassigned/2,
          add/2, add/3, remove/1, remove_all/0,
          assign/2, assign_next/1, assigned_to/1,
          release/1, release/2, release_orphaned/0, release_all/0,
          max_key/0]).
-include_lib("stdlib/include/qlc.hrl").
-record(egara_incoming_notification, { id, claimed = 0, fails = 0, term }).
-define(MAX_FAILURES, 5).

install(Nodes) ->
    rpc:multicall(Nodes, application, stop, [mnesia]),
    %% TODO: tune mnesia for large batches of writes; see -> http://streamhacker.com/2008/12/10/how-to-eliminate-mnesia-overload-events/
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    %% create index for claimed
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

remove_all() ->
    mnesia:clear_table(egara_incoming_notification).

notification(Key) ->
    F = fun() ->
                case mnesia:read(egara_incoming_notification, Key) of
                    [Record] -> Record;
                    [] -> #egara_incoming_notification{ id = 0 }
                end
        end,
    mnesia:activity(transaction, F).

assign(#egara_incoming_notification{ id = Key }, PID) when is_pid(PID) ->
    assign(Key, PID);

assign(Key, Fun) when is_function(Fun) ->
    F = fun() ->
                QH = qlc:q([ Rec || Rec <- mnesia:table(egara_incoming_notification),
                             Rec#egara_incoming_notification.fails < ?MAX_FAILURES, Rec#egara_incoming_notification.claimed =:= 0, Rec#egara_incoming_notification.id =:= Key ]),
                QC = qlc:cursor(QH),
                Answers = qlc:next_answers(QC, 1),
                case Answers of
                    [Record|_] -> mnesia:write(Record#egara_incoming_notification{ claimed = spawn(Fun) });
                    _ -> notfound
                end
        end,
    mnesia:activity(transaction, F);
assign(Key, PID) when is_pid(PID) ->
    F = fun() ->
                QH = qlc:q([ Rec || Rec <- mnesia:table(egara_incoming_notification),
                             Rec#egara_incoming_notification.fails < ?MAX_FAILURES, Rec#egara_incoming_notification.claimed =:= 0, Rec#egara_incoming_notification.id =:= Key ]),
                QC = qlc:cursor(QH),
                Answers = qlc:next_answers(QC, 1),
                case Answers of
                    [Record|_] -> mnesia:write(Record#egara_incoming_notification{ claimed = PID });
                    _ -> notfound
                end
        end,
    mnesia:activity(transaction, F).

release(Key, PID) ->
    %%TODO: make this more efficient by using qlc; currently scans whole table!
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
                    [#egara_incoming_notification{ term = Term }] -> mnesia:write(#egara_incoming_notification{ id = Key, term = Term, claimed = 0 }), ok;
                    [] -> false
                end
        end,
    mnesia:activity(transaction, F).

release_orphaned() ->
    F = fun() ->
                QH = qlc:q([ Record || #egara_incoming_notification{ claimed = PID } = Record <- mnesia:table(egara_incoming_notification),
                             is_pid(PID), process_info(PID) =:= undefined]),
                qlc:fold(fun(Record, N) ->
                                 Failed = Record#egara_incoming_notification.fails + 1,
                                 mnesia:write(Record#egara_incoming_notification{ claimed = 0, fails = Failed }), N + 1 end,
                         0, QH)
        end,
    mnesia:activity(transaction, F).

release_all() ->
    F = fun() ->
                QH = qlc:q([ Record || #egara_incoming_notification{ claimed = PID } = Record <- mnesia:table(egara_incoming_notification),
                             is_pid(PID) ]),
                qlc:fold(fun(Record, N) -> mnesia:write(Record#egara_incoming_notification{ claimed = 0 }), N + 1 end, 0, QH)
        end,
    mnesia:activity(transaction, F).

assigned_to(Key) ->
    F = fun() ->
                QH = qlc:q([ Rec || Rec <- mnesia:table(egara_incoming_notification),
                             Rec#egara_incoming_notification.id =:= Key]),
                QC = qlc:cursor(QH),
                Answers = qlc:next_answers(QC, 1),
                case Answers of
                    [Record|_] -> Record#egara_incoming_notification.claimed;
                    _ -> notfound
                end
        end,
    mnesia:activity(transaction, F).

assign_next(PID) when is_pid(PID) ->
    F = fun() ->
                QH = qlc:q([ Rec || Rec <- mnesia:table(egara_incoming_notification),
                             Rec#egara_incoming_notification.claimed =:= 0, Rec#egara_incoming_notification.fails < ?MAX_FAILURES]),
                QC = qlc:cursor(QH),
                Answers = qlc:next_answers(QC, 1),
                case Answers of
                    [Record|_] -> mnesia:write(Record#egara_incoming_notification{ claimed = PID }), { Record#egara_incoming_notification.id, Record#egara_incoming_notification.term };
                    _ -> notfound
                end
        end,
    mnesia:activity(transaction, F).

%% RequestedN -> the max number of notifications to process
%% C -> continuation to call with each notification
%% N -> countdown to 0
%% [] -> List of matching objects
do_next_unassigned(RequestedN, _, 0, _) ->
    RequestedN;
do_next_unassigned(RequestedN, _, N, []) ->
    RequestedN - N;
do_next_unassigned(RequestedN, C, N, [Key|T]) ->
    case C(Key#egara_incoming_notification.id, Key#egara_incoming_notification.term) of
        ok -> do_next_unassigned(RequestedN, C, N - 1, T);
        _ -> RequestedN - N
    end.

process_next_unassigned(1, C) when is_function(C, 2) ->
    F = fun() ->
                QH = qlc:q([ Rec || Rec <- mnesia:table(egara_incoming_notification),
                             Rec#egara_incoming_notification.claimed =:= 0]),
                QC = qlc:cursor(QH),
                qlc:next_answers(QC, 1)
        end,
    Answers = mnesia:activity(transaction, F),
    case Answers of
        [] -> notfound;
        [Record] -> C(Record#egara_incoming_notification.id, Record#egara_incoming_notification.term)
    end;
process_next_unassigned(NumRequested, C) when is_number(NumRequested), is_function(C, 2) ->
    F = fun() ->
                QH = qlc:q([ Rec || Rec <- mnesia:table(egara_incoming_notification),
                             Rec#egara_incoming_notification.claimed =:= 0]),
                QC = qlc:cursor(QH),
                qlc:next_answers(QC, NumRequested)
        end,
    Answers = mnesia:activity(transaction, F),
    NumProcessed = do_next_unassigned(NumRequested, C, NumRequested, Answers),
    lager:info("Processed ~p unassigned notifications", [NumProcessed]),
    NumRequested.

next_unassigned([]) ->
    none;
next_unassigned([H|_]) ->
    H#egara_incoming_notification.id;
next_unassigned(_) ->
    error.
next_unassigned() ->
    F = fun() ->
                QH = qlc:q([ Rec || Rec <- mnesia:table(egara_incoming_notification),
                             Rec#egara_incoming_notification.claimed =:= 0]),
                QC = qlc:cursor(QH),
                Answers = qlc:next_answers(QC, 1),
                next_unassigned(Answers)
        end,
    mnesia:activity(transaction, F).

max_key() ->
    F = fun() ->
                case mnesia:last(egara_incoming_notification) of
                    '$end_of_table' -> 0;
                    Key -> Key
                end
        end,
    mnesia:activity(transaction, F).

