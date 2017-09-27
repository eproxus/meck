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
-module(meck_ret_spec_tests).

-include_lib("eunit/include/eunit.hrl").

ret_spec_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun passthrough/0,
      fun explicit_exec/0,
      fun exec/0,
      fun deep_exec/0,
      fun invalid_arity_exec/0
     ]
    }.

setup() ->
    %% Given
    meck:new(meck_test_module),
    ok.

teardown(_) ->
    %% Cleanup
    meck:unload().

passthrough() ->
    %% When
    meck:expect(meck_test_module, c, 2, meck:passthrough()),
    %% Then
    ?assertEqual({1, 3}, meck_test_module:c(1, 3)).

explicit_exec() ->
    %% When
    meck:expect(meck_test_module, c, 2, meck:exec(fun(A, B) -> {B, A} end)),
    %% Then
    ?assertEqual({3, 1}, meck_test_module:c(1, 3)).

exec() ->
    %% When
    meck:expect(meck_test_module, c, 2, fun(A, B) -> {B, A} end),
    %% Then
    ?assertEqual({3, 1}, meck_test_module:c(1, 3)).

deep_exec() ->
    %% When
    meck:expect(meck_test_module, c, 2, meck_ret_spec:seq([fun(A, B) -> {B, A} end])),
    %% Then
    ?assertEqual({3, 1}, meck_test_module:c(1, 3)).

invalid_arity_exec() ->
    %% When
    meck:expect(meck_test_module, c, 2, meck_ret_spec:seq([fun(A, B) -> {B, A} end])),
    %% Then
    ?assertError(undef, meck_test_module:c(1, 2, 3)).
