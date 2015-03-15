-module(egara_imap_uidset_tests).
-include_lib("eunit/include/eunit.hrl").

single_value_test_() ->
    [
        ?_assert([1] == iterate_uidset(egara_imap_uidset:parse(<<"1">>))),
        ?_assert([20] == iterate_uidset(egara_imap_uidset:parse(<<"20">>))),
        ?_assert([147] == iterate_uidset(egara_imap_uidset:parse(<<"147">>)))
    ].

multiple_single_value_test_() ->
    [ ?_assert([1, 2] == iterate_uidset(egara_imap_uidset:parse(<<"1,2">>))) ].

range_test_() ->
    [
        ?_assert([1, 2, 3] == iterate_uidset(egara_imap_uidset:parse(<<"1:3">>))),
        ?_assert([3, 2, 1] == iterate_uidset(egara_imap_uidset:parse(<<"3:1">>))),
        ?_assert([1] == iterate_uidset(egara_imap_uidset:parse(<<"1:1">>)))
    ].

multiple_range_test_() ->
    [ ?_assert([1, 2, 3, 10, 11, 12, 13, 14, 15] == iterate_uidset(egara_imap_uidset:parse(<<"1:3,10:15">>))) ].

mix_single_and_range_test_() ->
    [
        ?_assert([1, 3, 4, 5] == iterate_uidset(egara_imap_uidset:parse(<<"1,3:5">>))),
        ?_assert([1, 3, 4, 5, 10] == iterate_uidset(egara_imap_uidset:parse(<<"1,3:5,10">>))),
        ?_assert([1, 3, 4, 5, 10, 20, 21, 22, 23, 30] == iterate_uidset(egara_imap_uidset:parse(<<"1,3:5,10,20:23,30">>)))
    ].

mix_single_and_range_with_whitespace_test_() ->
    [
        ?_assert(badarg == egara_imap_uidset:parse(<<"1, 3:5">>)),
        ?_assert(badarg == egara_imap_uidset:parse(<<"1,3:5 ,10">>)),
        ?_assert(badarg == egara_imap_uidset:parse(<<"1,3 :5,10,20:  23,30">>))
    ].

bad_uidsets_tests_() ->
    [
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"-1,3:5">>))),
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"alpha">>))),
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"1,a,3:5">>))),
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"1,3:5,a">>))),
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"1;2">>))),
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"1, 2">>))),
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"1:3:5">>))),
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"-11,3:5">>))),
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"11,-3:5">>))),
        ?_assert(badarg == iterate_uidset(egara_imap_uidset:parse(<<"11,3:-5">>)))
    ].


iterate_uidset(badarg) ->
    badarg;
iterate_uidset(UidSet) ->
    %%io:fwrite("we are going to iterate over ~p~n", [UidSet]),
    lists:reverse(iterate_uidset(egara_imap_uidset:next_uid(UidSet), [])).
iterate_uidset({ none, _UidSet }, Acc) -> Acc;
iterate_uidset({ Uid, UidSet }, Acc) -> iterate_uidset(egara_imap_uidset:next_uid(UidSet), [Uid|Acc]).

