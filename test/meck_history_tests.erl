%%%============================================================================
%%% Copyright 2013 Maxim Vladimirsky
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%============================================================================

-module(meck_history_tests).

-include_lib("eunit/include/eunit.hrl").

num_calls_with_arity_test() ->
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 2, ok),
    meck:expect(test, foo, 3, ok),
    %% When
    test:foo(1, 2, 3),
    test:foo(1, 2),
    test:foo(1, 2, 3),
    test:foo(1, 2, 3),
    test:foo(1, 2),
    %% Then
    ?assertMatch(2, meck:num_calls(test, foo, 2)),
    ?assertMatch(3, meck:num_calls(test, foo, 3)),
    ?assertMatch(0, meck:num_calls(test, foo, 4)),
    %% Clean
    meck:unload().

capture_different_positions_test() ->
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 3, ok),
    meck:expect(test, foo, 4, ok),
    meck:expect(test, bar, 3, ok),
    test:foo(1001, 2001, 3001, 4001),
    test:bar(1002, 2002, 3002),
    test:foo(1003, 2003, 3003),
    test:bar(1004, 2004, 3004),
    test:foo(1005, 2005, 3005),
    test:foo(1006, 2006, 3006),
    test:bar(1007, 2007, 3007),
    test:foo(1008, 2008, 3008),
    %% When/Then
    ?assertMatch(2003, meck:capture(first, test, foo, ['_', '_', '_'], 2)),
    ?assertMatch(2008, meck:capture(last, test, foo, ['_', '_', '_'], 2)),
    ?assertMatch(2006, meck:capture(3, test, foo, ['_', '_', '_'], 2)),
    ?assertError(not_found, meck:capture(5, test, foo, ['_', '_', '_'], 2)),
    %% Clean
    meck:unload().

capture_different_args_specs_test() ->
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 2, ok),
    meck:expect(test, foo, 3, ok),
    meck:expect(test, foo, 4, ok),
    meck:expect(test, bar, 3, ok),
    test:foo(1001, 2001, 3001, 4001),
    test:bar(1002, 2002, 3002),
    test:foo(1003, 2003, 3003),
    test:bar(1004, 2004, 3004),
    test:foo(1005, 2005),
    test:foo(1006, 2006, 3006),
    test:bar(1007, 2007, 3007),
    test:foo(1008, 2008, 3008),
    %% When/Then
    ?assertMatch(2001, meck:capture(first, test, foo, '_', 2)),
    ?assertMatch(2003, meck:capture(first, test, foo, 3, 2)),
    ?assertMatch(2005, meck:capture(first, test, foo, ['_', '_'], 2)),
    ?assertMatch(2006, meck:capture(first, test, foo, [1006, '_', '_'], 2)),
    ?assertMatch(2008, meck:capture(first, test, foo, ['_', '_', meck:is(hamcrest_matchers:greater_than(3006))], 2)),
    %% Clean
    meck:unload().
