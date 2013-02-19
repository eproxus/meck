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
