%%%============================================================================
%%% Copyright 2010-2017 Adam Lindberg, 2010-2011 Erlang Solutions Ltd
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
-module(meck_expect_tests).

-include_lib("eunit/include/eunit.hrl").

expect_explicit_values_test() ->
    %% When
    E = meck_expect:new(blah, [1000, a, {1002, [{<<"b">>, 1003}]}], 2001),
    %% Then
    V2001 = meck_ret_spec:val(2001),
    ?assertMatch({V2001, _},
                 meck_expect:fetch_result([1000, a, {1002, [{<<"b">>, 1003}]}], E)),
    ?assertMatch({undefined, _},
                 meck_expect:fetch_result([1001, a, {1002, [{<<"b">>, 1003}]}], E)),
    ?assertMatch({undefined, _},
                 meck_expect:fetch_result([1000, b, {1002, [{<<"b">>, 1003}]}], E)),
    ?assertMatch({undefined, _},
                 meck_expect:fetch_result([1000, a, {1003, [{<<"b">>, 1003}]}], E)),
    ?assertMatch({undefined, _},
                 meck_expect:fetch_result([1000, a, {1002, [{<<"c">>, 1003}]}], E)),
    ?assertMatch({undefined, _},
                 meck_expect:fetch_result([1000, a, {1002, [{<<"b">>, 1004}]}], E)).

expect_wildcard_test() ->
    %% When
    E = meck_expect:new(blah, [1000, '_', {'_', [{'_', 1003}]}], 2001),
    %% Then
    V2001 = meck_ret_spec:val(2001),
    ?assertMatch({V2001, _},
                 meck_expect:fetch_result([1000, a, {1002, [{<<"b">>, 1003}]}], E)),
    ?assertMatch({undefined, _},
                 meck_expect:fetch_result([1001, a, {1002, [{<<"b">>, 1003}]}], E)),
    ?assertMatch({V2001, _},
                 meck_expect:fetch_result([1000, b, {1002, [{<<"b">>, 1003}]}], E)),
    ?assertMatch({V2001, _},
                 meck_expect:fetch_result([1000, a, {1003, [{<<"b">>, 1003}]}], E)),
    ?assertMatch({V2001, _},
                 meck_expect:fetch_result([1000, a, {1002, [{[1, {2}, 3], 1003}]}], E)),
    ?assertMatch({undefined, _},
                 meck_expect:fetch_result([1000, a, {1002, [{<<"b">>, 1004}]}], E)).

expect_matchers_test() ->
    %% Given
    Is1003 = meck_matcher:new(fun(X) -> X == 1003 end),
    LessThen1004 = meck_matcher:new(fun(X) -> X < 1004 end),
    %% When
    E = meck_expect:new(blah, [Is1003, LessThen1004], 2001),
    %% Then
    V2001 = meck_ret_spec:val(2001),
    ?assertMatch({V2001, _},     meck_expect:fetch_result([1003, 1002], E)),
    ?assertMatch({V2001, _},     meck_expect:fetch_result([1003, 1003], E)),
    ?assertMatch({undefined, _}, meck_expect:fetch_result([1003, 1004], E)),
    ?assertMatch({undefined, _}, meck_expect:fetch_result([1002, 1002], E)).

expect_with_matchers_multiclause_test() ->
    %% Given
    Is1003 = meck_matcher:new(fun(X) -> X == 1003 end),
    LessThen1004 = meck_matcher:new(fun(X) -> X < 1004 end),
    %% When
    E = meck_expect:new(blah, [{['_', Is1003, 1004],         2001},
                               {['_', Is1003, LessThen1004], 2002},
                               {['_', '_',    LessThen1004], 2003}]),
    %% Then
    V2001 = meck_ret_spec:val(2001),
    ?assertMatch({V2001, _},
                 meck_expect:fetch_result([1002, 1003, 1004], E)),
    V2002 = meck_ret_spec:val(2002),
    ?assertMatch({V2002, _},
                 meck_expect:fetch_result([1002, 1003, 1003], E)),
    V2003 = meck_ret_spec:val(2003),
    ?assertMatch({V2003, _},
                 meck_expect:fetch_result([1002, 1004, 1003], E)),
    ?assertMatch({undefined, _},
                 meck_expect:fetch_result([1002, 1003, 1005], E)).

expect_with_matchers_masked_clause_test() ->
    %% Given
    Is1003 = meck_matcher:new(fun(X) -> X == 1003 end),
    LessThen1004 = meck_matcher:new(fun(X) -> X < 1004 end),
    %% When
    E = meck_expect:new(blah, [{[Is1003, LessThen1004], 2001},
                               {[Is1003, Is1003], 2002}]),
    %% Then
    V2001 = meck_ret_spec:val(2001),
    ?assertMatch({V2001, _}, meck_expect:fetch_result([1003, 1003], E)).

expect_with_arity_test() ->
    %% When
    E = meck_expect:new(foo, [{2, 2001}]),
    %% Then
    V2001 = meck_ret_spec:val(2001),
    ?assertMatch({V2001, _}, meck_expect:fetch_result([1, 2], E)),
    ?assertMatch({undefined, _}, meck_expect:fetch_result([1, 2, 3], E)).
