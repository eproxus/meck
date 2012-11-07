%%%============================================================================
%%% Copyright 2010 Erlang Solutions Ltd.
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


passthrough_test() ->
    %% Given
    meck:new(meck_test_module),
    %% When
    meck:expect(meck_test_module, c, 2, meck:passthrough()),
    %% Then
    ?assertEqual({1, 3}, meck_test_module:c(1, 3)),
    %% Cleanup
    meck:unload(meck_test_module).


fun_test() ->
    %% Given
    meck:new(meck_test_module),
    %% When
    meck:expect(meck_test_module, c, 2, meck:func(fun(A, B) -> {B, A} end)),
    %% Then
    ?assertEqual({3, 1}, meck_test_module:c(1, 3)),
    %% Cleanup
    meck:unload(meck_test_module).
