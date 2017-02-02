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
-module(meck_args_matcher_tests).

-include_lib("eunit/include/eunit.hrl").

from_wildcard_test() ->
    ArgsMatcher = meck_args_matcher:new('_'),
    ?assertMatch(true, meck_args_matcher:match([], ArgsMatcher)),
    ?assertMatch(true, meck_args_matcher:match([1], ArgsMatcher)),
    ?assertMatch(true, meck_args_matcher:match([1, 2, 3], ArgsMatcher)).

from_arity_test() ->
    ArgsMatcher = meck_args_matcher:new(3),
    ?assertMatch(true,  meck_args_matcher:match([1, 2, 3], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, 2], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, 2, 3, 4], ArgsMatcher)).

from_zero_arity_test() ->
    ArgsMatcher = meck_args_matcher:new(0),
    ?assertMatch(true,  meck_args_matcher:match([], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, 2, 3], ArgsMatcher)).

from_args_test() ->
    ArgsMatcher = meck_args_matcher:new([1, {2, [<<"3">>]}]),
    ?assertMatch(true,  meck_args_matcher:match([1, {2, [<<"3">>]}], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, {2, [<<"3">>]}, 1], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, {0, [<<"3">>]}], ArgsMatcher)).

from_empty_args_test() ->
    ArgsMatcher = meck_args_matcher:new([]),
    ?assertMatch(true,  meck_args_matcher:match([], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, 2, 3], ArgsMatcher)).

matcher_featured_test() ->
    ArgsSpec = [meck:is(fun(X) -> X == 1 end),
                2,
                meck:is(fun(X) -> X == 3 end),
                {4, [5, '_'], <<"7">>}],
    ArgsMatcher = meck_args_matcher:new(ArgsSpec),
    ?assertMatch(true,  meck_args_matcher:match([1, 2, 3, {4, [5, 6], <<"7">>}], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([0, 2, 3, {4, [5, 6], <<"7">>}], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, 0, 3, {4, [5, 6], <<"7">>}], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, 2, 0, {4, [5, 6], <<"7">>}], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, 2, 3, {0, [5, 6], <<"7">>}], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, 2, 3, {4, [0, 6], <<"7">>}], ArgsMatcher)),
    ?assertMatch(true,  meck_args_matcher:match([1, 2, 3, {4, [5, 0], <<"7">>}], ArgsMatcher)),
    ?assertMatch(false, meck_args_matcher:match([1, 2, 3, {4, [5, 6], <<"0">>}], ArgsMatcher)).
