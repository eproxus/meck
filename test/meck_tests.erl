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
      fun stacktrace_function_clause_/0,
      fun change_func_/0,
      fun caller_does_not_crash_on_reload_/0,
      fun call_original_undef_/0]}.

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

stacktrace_() ->
    meck:expect(mymod, test, fun() -> erlang:error(test_error) end),
    try 
        mymod:test(),
        throw(failed)
    catch
        error:test_error ->
            ?assert(lists:any(fun({mymod, test, []}) -> true;
                                 (_)                 -> false end,
                              erlang:get_stacktrace()))
    end.

stacktrace_function_clause_() ->
    meck:expect(mymod, test, fun(1) -> ok end),
    try 
        mymod:test(error),
        throw(failed)
    catch
        error:function_clause ->
            Stacktrace = erlang:get_stacktrace(),
            ?assert(lists:any(fun({mymod, test, [error]}) -> true;
                                 (_)                      -> false end,
                              Stacktrace))
    end.


call_undef_() ->
    meck:expect(mymod, test, fun(hest, 1) -> apa end),
    ?assertError(undef, mymod:test(hest)).

caller_does_not_crash_on_reload_() ->
    meck:expect(mymod, test, fun() -> timer:sleep(infinity) end),
    Pid = spawn(fun() -> mymod:test() end),
    meck:expect(mymod, new1, fun() -> ok end),
    meck:expect(mymod, new2, fun() -> ok end),
    meck:expect(mymod, new3, fun() -> ok end),
    ?assertEqual(true, is_process_alive(Pid)).

change_func_() ->
    meck:expect(mymod, test, fun() -> 1 end),
    ?assertEqual(1, mymod:test()),
    MTime = proplists:get_value(time, mymod:module_info(compile)),
    % recompile will result in increased module_info time
    timer:sleep(1100),
    meck:expect(mymod, test, fun() -> 2 end),
    ?assertEqual(2, mymod:test()),
    ?assertEqual(MTime, proplists:get_value(time, mymod:module_info(compile))).

call_original_test() ->
    meck:new(meck_test_module),
    meck:expect(meck_test_module, a, fun() -> c end),
    meck:expect(meck_test_module, b, fun() -> meck:passthrough([]) end),
    ?assertEqual(c, meck_test_module:a()),
    ?assertEqual(b, meck_test_module:b()),
    meck:unload(meck_test_module).

call_original_undef_() ->
    meck:expect(mymod, test, fun() -> meck:passthrough([]) end),
    ?assertError(undef, mymod:test()).

unload_renamed_original_test() ->
    meck:new(meck_test_module),
    meck:unload(meck_test_module),
    ?assertEqual(false, code:is_loaded(meck_test_module_meck_original)).

original_no_file_test() ->
    {ok, Mod, Beam} = compile:forms([{attribute, 1, module, meck_not_on_disk}]),
    {module, Mod} = code:load_binary(Mod, "", Beam),
    ?assertEqual(ok, meck:new(meck_not_on_disk)).

original_has_no_object_code_test() ->
    {ok, Mod, Beam} = compile:forms([{attribute, 1, module, meck_on_disk}]),
    ok = file:write_file("meck_on_disk.beam", Beam),
    {module, Mod} = code:load_binary(Mod, "meck_on_disk.beam", Beam),
    ?assertEqual(ok, meck:new(meck_on_disk)),
    ok = file:delete("meck_on_disk.beam").

passthrough_nonexisting_module_test() ->
    meck:new(mymod, [passthrough]),
    meck:expect(mymod, test, fun() -> ok end),
    ?assertEqual(ok, mymod:test()),
    meck:unload(mymod).
    
passthrough_test() ->
    meck:new(meck_test_module, [passthrough]),
    meck:expect(meck_test_module, a, fun() -> c end),
    ?assertEqual(c, meck_test_module:a()),
    ?assertEqual(b, meck_test_module:b()),
    meck:unload(meck_test_module).

% @doc The mocked module is unloaded if the meck process crashes.
unload_when_crashed_test() ->
    meck:new(mymod),
    ?assertMatch({file, _}, code:is_loaded(mymod)),
    SaltedName = mymod_meck,
    Pid = whereis(SaltedName),
    ?assertEqual(true, is_pid(Pid)),
    unlink(Pid),
    exit(Pid, expected_test_exit),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(SaltedName)),
    ?assertEqual(false, code:is_loaded(mymod)).

%% @doc Exception is thrown when you run expect on a non-existing module.
expect_without_new_test() ->
    ?assertError({not_mocked, mymod},
                 meck:expect(mymod, test, fun() -> ok end)).
                                                           


