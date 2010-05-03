%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(meck_tests).

-include_lib("eunit/include/eunit.hrl").

meck_test_() ->
    {foreach, fun setup/0, fun teardown/1, 
     [fun new_/0,
      fun unload_/0,
      fun double_new_/0,
      fun validate_/0,
      fun expect_/0,
      fun exports_/0,
      fun call_return_value_/0,
      fun call_argument_/0,
      fun call_undef_/0,
      fun call_function_clause_/0,
      fun validate_unexpected_error_/0,
      fun validate_expected_error_/0,
      fun validate_chained_/0,
      fun stacktrace_/0,
      fun change_func_/0]}.

setup() ->
    % Uncomment to run tests with dbg:
    % dbg:tracer(),
    % dbg:p(all, call),
    % dbg:tpl(meck, []),
    meck:new(mymod),
    mymod.

teardown(Module) ->
    catch meck:unload(Module).


new_() ->
    Info = mymod:module_info(),
    ?assert(is_list(Info)).

unload_() ->
    meck:unload(mymod),
    ?assertEqual(false, code:is_loaded(mymod)).

double_new_() ->
    ?assertError({already_started, _}, meck:new(mymod)).

validate_() ->
    ?assertEqual(true, meck:validate(mymod)).

expect_() ->
    meck:expect(mymod, test, fun() -> ok end),
    ?assertEqual(true, meck:validate(mymod)).

exports_() ->
    meck:expect(mymod, test1, fun() -> ok end),
    meck:expect(mymod, test2, fun() -> ok end),
    ?assertEqual(0, proplists:get_value(test1, mymod:module_info(exports))),
    ?assertEqual(0, proplists:get_value(test2, mymod:module_info(exports))),
    ?assertEqual(true, meck:validate(mymod)).

call_return_value_() ->
    meck:expect(mymod, test, fun() -> apa end),
    ?assertEqual(apa, mymod:test()),
    ?assertEqual(true, meck:validate(mymod)).

call_argument_() ->
    meck:expect(mymod, test, fun(hest, 1) -> apa end),
    ?assertEqual(apa, mymod:test(hest, 1)),
    ?assertEqual(true, meck:validate(mymod)).

call_function_clause_() ->
    meck:expect(mymod, test, fun(hest, 1) -> apa end),
    ?assertError(function_clause, mymod:test(hest, 2)),
    ?assertEqual(false, meck:validate(mymod)).

validate_unexpected_error_() ->
    meck:expect(mymod, test, fun(hest, 1) -> erlang:error(timeout) end),
    ?assertError(timeout, mymod:test(hest, 1)),
    ?assertEqual(false, meck:validate(mymod)).

validate_expected_error_() ->
    meck:expect(mymod, test, fun(hest, 1) -> 
                                     meck:exception(error, timeout)
                             end),
    ?assertError(timeout, mymod:test(hest, 1)),
    ?assertEqual(true, meck:validate(mymod)).

validate_chained_() ->
    meck:new(mymod2),
    meck:expect(mymod2, test, fun() ->
                                      meck:exception(error, test_error)
                              end),
    meck:expect(mymod, test, fun() ->
                                     mymod2:test()
                             end),
    ?assertError(test_error, mymod:test()),
    ?assertEqual(false, meck:validate(mymod)),
    ?assertEqual(true, meck:validate(mymod2)),
    meck:unload(mymod2).

%% TODO: does not verify stacktrace, only prints it
stacktrace_() ->
    meck:expect(mymod, test, fun() ->
                                     meck:exception(throw, test_throw)
                             end),
    try 
        mymod:test(),
        throw(failed)
    catch
        throw:test_throw ->
            %?debugFmt("\n~p\n\n", [erlang:get_stacktrace()]),
            ok
    end.

call_undef_() ->
    meck:expect(mymod, test, fun(hest, 1) -> apa end),
    ?assertError(undef, mymod:test(hest)).

change_func_() ->
    meck:expect(mymod, test, fun() -> 1 end),
    ?assertEqual(1, mymod:test()),
    MTime = proplists:get_value(time, mymod:module_info(compile)),
    % recompile will result in increased module_info time
    timer:sleep(1100),
    meck:expect(mymod, test, fun() -> 2 end),
    ?assertEqual(2, mymod:test()),
    ?assertEqual(MTime, proplists:get_value(time, mymod:module_info(compile))).

% @doc The mock module is unloaded if the mock process crashes.
unload_when_crashed_test() ->
    meck:new(mymod),
    ?assertMatch({file, _}, code:is_loaded(mymod)),
    SaltedName = mymod_meck,
    Pid = whereis(SaltedName),
    ?assertEqual(true, is_pid(Pid)),
    unlink(Pid),
    exit(Pid, expected_test_exit),
    timer:sleep(10),
    ?assertEqual(undefined, whereis(SaltedName)),
    ?assertEqual(false, code:is_loaded(mymod)).

%% @doc Exception is thrown when you run expect on a non-existing module.
expect_without_new_test() ->
    ?assertError({not_mocked, mymod},
                 meck:expect(mymod, test, fun() -> ok end)).
                                                           


