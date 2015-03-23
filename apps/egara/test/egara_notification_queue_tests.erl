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

-module(egara_notification_queue_tests).
-include_lib("eunit/include/eunit.hrl").
-define(FAILURE_KEY, <<"FAILURE_KEY">>).

% c("apps/egara/test/egara_notification_queue_tests.erl"). eunit:test(egara_notification_queue).
assignnext_failure_test_() ->
    {
      "Tests that a notification will be moved out of rotation after too many failures with assign_next/1",
      { setup, fun assignnext_failure_start/0, fun assignnext_failure_stop/1, fun assignnext_failure_gen/1 }
    }.

assignnext_failure_start() ->
    egara_notification_queue:remove(?FAILURE_KEY),
    egara_notification_queue:add(?FAILURE_KEY, <<"foobar">>),
    ?FAILURE_KEY.

assignnext_failure_stop(Key) ->
    egara_notification_queue:remove(Key).

assignnext_failure_fold(_Key, BadPid, Acc) ->
    egara_notification_queue:release_orphaned(),
    Res = egara_notification_queue:assign_next(BadPid),
    [?_assertNotEqual(notfound, Res)|Acc].

assignnext_failure_gen(Key) ->
    BadPid = list_to_pid("<0.99.99>"),
    Successes = lists:foldl(fun(_, Acc) -> assignnext_failure_fold(Key, BadPid, Acc) end, [], lists:seq(1, 5)),
    egara_notification_queue:release_orphaned(),
    Res = egara_notification_queue:assign_next(BadPid),
    [?_assertEqual(notfound, Res)|Successes].

assign_failure_test_() ->
    {
      "Tests that a notification will be moved out of rotation after too many failures with assign/2",
      { setup, fun assign_failure_start/0, fun assign_failure_stop/1, fun assign_failure_gen/1 }
    }.

assign_failure_start() ->
    egara_notification_queue:remove(?FAILURE_KEY),
    egara_notification_queue:add(?FAILURE_KEY, <<"foobar">>),
    ?FAILURE_KEY.

assign_failure_stop(Key) ->
    egara_notification_queue:remove(Key).

assign_failure_fold(Key, BadPid, Acc) ->
    egara_notification_queue:release_orphaned(),
    Res = egara_notification_queue:assign(Key, BadPid),
    [?_assertNotEqual(notfound, Res)|Acc].

assign_failure_gen(Key) ->
    BadPid = list_to_pid("<0.99.99>"),
    Successes = lists:foldl(fun(_, Acc) -> assign_failure_fold(Key, BadPid, Acc) end, [], lists:seq(1, 5)),
    egara_notification_queue:release_orphaned(),
    Res = egara_notification_queue:assign(Key, BadPid),
    [?_assertEqual(notfound, Res)|Successes].

