%%%============================================================================
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
-module(meck_matcher_tests).

-include_lib("eunit/include/eunit.hrl").

match_predicate_test() ->
    Matcher = meck_matcher:new(fun(X) -> X == 1000 end),
    ?assertMatch(true, meck_matcher:match_ignore(1000, Matcher)),
    ?assertMatch(false, meck_matcher:match_ignore(1001, Matcher)).

match_predicate_not_bool_test() ->
    Matcher = meck_matcher:new(fun(1000) -> true;
        (Other) -> Other
                               end),
    ?assertMatch(true, meck_matcher:match_ignore(1000, Matcher)),
    ?assertMatch(false, meck_matcher:match_ignore(1001, Matcher)).

match_hamcrest_test() ->
    Matcher = meck_matcher:new(hamcrest_matchers:equal_to(1000)),
    ?assertMatch(true, meck_matcher:match_ignore(1000, Matcher)),
    ?assertMatch(false, meck_matcher:match_ignore(1001, Matcher)).

match_not_matcher_test() ->
    ?assertMatch(true, meck_matcher:match_ignore(something, '_')),
    ?assertMatch(true, meck_matcher:match_ignore({1, [2, 3], undefined}, {1, [2, 3], undefined})).

predicate_wrong_arity_test() ->
    Predicate = fun(X, Y) -> X == Y end,
    ?assertError(_, meck_matcher:new(Predicate)).

is_matcher_test() ->
    ?assertMatch(true, meck_matcher:is_matcher(meck_matcher:new(fun(X) -> X == 1000 end))),
    ?assertMatch(false, meck_matcher:is_matcher(fun(X) -> X == 1000 end)),
    ?assertMatch(true, meck_matcher:is_matcher(meck_matcher:new(hamcrest_matchers:equal_to(1000)))),
    ?assertMatch(false, meck_matcher:is_matcher(hamcrest_matchers:equal_to(1000))),
    ?assertMatch(false, meck_matcher:is_matcher(blah)).
