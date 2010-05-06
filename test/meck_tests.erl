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

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

meck_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun new_/1,
                           fun unload_/1,
                           fun double_new_/1,
                           fun validate_/1,
                           fun expect_/1,
                           fun exports_/1,
                           fun call_return_value_/1,
                           fun call_argument_/1,
                           fun call_undef_/1,
                           fun call_function_clause_/1,
                           fun validate_unexpected_error_/1,
                           fun validate_expected_error_/1,
                           fun validate_chained_/1,
                           fun stacktrace_/1,
                           fun stacktrace_function_clause_/1,
                           fun change_func_/1,
                           fun caller_does_not_crash_on_reload_/1,
                           fun call_original_undef_/1,
                           fun history_empty_/1,
                           fun history_call_/1,
                           fun history_throw_/1,
                           fun history_throw_fun_/1,
                           fun history_exit_/1,
                           fun history_error_/1,
                           fun history_error_args_/1,
                           fun history_meck_throw_/1,
                           fun history_meck_throw_fun_/1,
                           fun history_meck_exit_/1,
                           fun history_meck_error_/1]]}.

setup() ->
    % Uncomment to run tests with dbg:
    % dbg:tracer(),
    % dbg:p(all, call),
    % dbg:tpl(meck, []),
    meck:new(mymod),
    mymod.

teardown(Module) ->
    catch meck:unload(Module).

%% --- Tests using setup and teardown ------------------------------------------

new_(Mod) ->
    Info = Mod:module_info(),
    ?assert(is_list(Info)).

unload_(Mod) ->
    meck:unload(Mod),
    ?assertEqual(false, code:is_loaded(Mod)).

double_new_(Mod) ->
    ?assertError({already_started, _}, meck:new(Mod)).

validate_(Mod) ->
    ?assertEqual(true, meck:validate(Mod)).

expect_(Mod) ->
    meck:expect(Mod, test, fun() -> ok end),
    ?assertEqual(true, meck:validate(Mod)).

exports_(Mod) ->
    meck:expect(Mod, test1, fun() -> ok end),
    meck:expect(Mod, test2, fun() -> ok end),
    ?assertEqual(0, proplists:get_value(test1, Mod:module_info(exports))),
    ?assertEqual(0, proplists:get_value(test2, Mod:module_info(exports))),
    ?assertEqual(true, meck:validate(Mod)).

call_return_value_(Mod) ->
    meck:expect(Mod, test, fun() -> apa end),
    ?assertEqual(apa, Mod:test()),
    ?assertEqual(true, meck:validate(Mod)).

call_argument_(Mod) ->
    meck:expect(Mod, test, fun(hest, 1) -> apa end),
    ?assertEqual(apa, Mod:test(hest, 1)),
    ?assertEqual(true, meck:validate(Mod)).

call_function_clause_(Mod) ->
    meck:expect(Mod, test, fun(hest, 1) -> apa end),
    ?assertError(function_clause, Mod:test(hest, 2)),
    ?assertEqual(false, meck:validate(Mod)).

validate_unexpected_error_(Mod) ->
    meck:expect(Mod, test, fun(hest, 1) -> erlang:error(timeout) end),
    ?assertError(timeout, Mod:test(hest, 1)),
    ?assertEqual(false, meck:validate(Mod)).

validate_expected_error_(Mod) ->
    meck:expect(Mod, test, fun(hest, 1) -> 
                                     meck:exception(error, timeout)
                             end),
    ?assertError(timeout, Mod:test(hest, 1)),
    ?assertEqual(true, meck:validate(Mod)).

validate_chained_(Mod) ->
    meck:new(mymod2),
    meck:expect(mymod2, test, fun() ->
                                      meck:exception(error, test_error)
                              end),
    meck:expect(Mod, test, fun() ->
                                     mymod2:test()
                             end),
    ?assertError(test_error, Mod:test()),
    ?assertEqual(false, meck:validate(Mod)),
    ?assertEqual(true, meck:validate(mymod2)),
    meck:unload(mymod2).

stacktrace_(Mod) ->
    meck:expect(Mod, test, fun() -> erlang:error(test_error) end),
    try 
        Mod:test(),
        throw(failed)
    catch
        error:test_error ->
            ?assert(lists:any(fun({M, test, []}) when M == Mod -> true;
                                 (_)                 -> false end,
                              erlang:get_stacktrace()))
    end.

stacktrace_function_clause_(Mod) ->
    meck:expect(Mod, test, fun(1) -> ok end),
    try 
        Mod:test(error),
        throw(failed)
    catch
        error:function_clause ->
            Stacktrace = erlang:get_stacktrace(),
            ?assert(lists:any(fun({M, test, [error]}) when M == Mod -> true;
                                 (_)                      -> false end,
                              Stacktrace))
    end.


call_undef_(Mod) ->
    meck:expect(Mod, test, fun(hest, 1) -> apa end),
    ?assertError(undef, Mod:test(hest)).

caller_does_not_crash_on_reload_(Mod) ->
    meck:expect(Mod, test, fun() -> timer:sleep(infinity) end),
    Pid = spawn(fun() -> Mod:test() end),
    meck:expect(Mod, new1, fun() -> ok end),
    meck:expect(Mod, new2, fun() -> ok end),
    meck:expect(Mod, new3, fun() -> ok end),
    ?assertEqual(true, is_process_alive(Pid)).

change_func_(Mod) ->
    meck:expect(Mod, test, fun() -> 1 end),
    ?assertEqual(1, Mod:test()),
    MTime = proplists:get_value(time, Mod:module_info(compile)),
    % recompile will result in increased module_info time
    timer:sleep(1100),
    meck:expect(Mod, test, fun() -> 2 end),
    ?assertEqual(2, Mod:test()),
    ?assertEqual(MTime, proplists:get_value(time, Mod:module_info(compile))).

call_original_undef_(Mod) ->
    meck:expect(Mod, test, fun() -> meck:passthrough([]) end),
    ?assertError(undef, Mod:test()).

history_empty_(Mod) ->
    ?assertEqual([], meck:history(Mod)).

history_call_(Mod) ->
    meck:expect(Mod, test, fun() -> ok end),
    meck:expect(Mod, test2, fun(_, _) -> result end),
    Mod:test(),
    Mod:test2(a, b),
    ?assertEqual([{{Mod, test, []}, ok},
                  {{Mod, test2, [a, b]}, result}], meck:history(Mod)).

history_throw_(Mod) ->
    meck:expect(Mod, test, fun() -> throw(test_exception) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, throw, test_exception, _Stacktrace}],
                 meck:history(Mod)).

history_throw_fun_(Mod) ->
    Fun = fun() -> exception_fun end,
    meck:expect(Mod, test, fun() -> throw(Fun) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, throw, Fun, _Stacktrace}],
                 meck:history(Mod)).

history_exit_(Mod) ->
    meck:expect(Mod, test, fun() -> exit(test_exit) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, exit, test_exit, _Stacktrace}],
                 meck:history(Mod)).

history_error_(Mod) ->
    meck:expect(Mod, test, fun() -> erlang:error(test_error) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, error, test_error, _Stacktrace}],
                 meck:history(Mod)).

history_error_args_(Mod) ->
    meck:expect(Mod, test, fun() -> erlang:error(test_error, [fake_args]) end),
    catch Mod:test(),
    History = meck:history(Mod),
    ?assertMatch([{{Mod, test, []}, error, test_error, _Stacktrace}],
                 meck:history(Mod)),
    [{_MFA, error, test_error, Stacktrace}] = History,
    ?assert(lists:any(fun({_M, _F, [fake_args]}) -> true;
                         (_) -> false end, Stacktrace)).

history_meck_throw_(Mod) ->
    meck:expect(Mod, test, fun() -> meck:exception(throw, test_exception) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, throw, test_exception, _Stacktrace}],
                 meck:history(Mod)).

history_meck_throw_fun_(Mod) ->
    Fun = fun() -> exception_fun end,
    meck:expect(Mod, test, fun() -> meck:exception(throw, Fun) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, throw, Fun, _Stacktrace}],
                 meck:history(Mod)).

history_meck_exit_(Mod) ->
    meck:expect(Mod, test, fun() -> meck:exception(exit, test_exit) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, exit, test_exit, _Stacktrace}],
                 meck:history(Mod)).

history_meck_error_(Mod) ->
    meck:expect(Mod, test, fun() -> meck:exception(error, test_error) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, error, test_error, _Stacktrace}],
                 meck:history(Mod)).

%% --- Tests with own setup ----------------------------------------------------

call_original_test() ->
    ?assertEqual({module, meck_test_module}, code:load_file(meck_test_module)),
    meck:new(meck_test_module),
    ?assertEqual({file, ""}, code:is_loaded(meck_test_module_meck_original)),
    meck:expect(meck_test_module, a, fun() -> c end),
    meck:expect(meck_test_module, b, fun() -> meck:passthrough([]) end),
    ?assertEqual(c, meck_test_module:a()),
    ?assertEqual(b, meck_test_module:b()),
    meck:unload(meck_test_module).

unload_renamed_original_test() ->
    meck:new(meck_test_module),
    meck:unload(meck_test_module),
    ?assertEqual(false, code:is_loaded(meck_test_module_meck_original)).

original_no_file_test() ->
    {ok, Mod, Beam} = compile:forms([{attribute, 1, module, meck_not_on_disk}]),
    {module, Mod} = code:load_binary(Mod, "", Beam),
    ?assertEqual(ok, meck:new(meck_not_on_disk)),
    meck:unload(meck_not_on_disk).

original_has_no_object_code_test() ->
    {ok, Mod, Beam} = compile:forms([{attribute, 1, module, meck_on_disk}]),
    ok = file:write_file("meck_on_disk.beam", Beam),
    {module, Mod} = code:load_binary(Mod, "meck_on_disk.beam", Beam),
    ?assertEqual(ok, meck:new(meck_on_disk)),
    ok = file:delete("meck_on_disk.beam"),
    meck:unload(meck_on_disk).

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
    ?assertEqual({1, 2}, meck_test_module:c(1, 2)),
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
                                                           
history_passthrough_test() ->
    meck:new(meck_test_module, [passthrough]),
    meck:expect(meck_test_module, a, fun() -> c end),
    c = meck_test_module:a(),
    ?assertEqual([{{meck_test_module, a, []}, c}],
                 meck:history(meck_test_module)),
    meck:unload(meck_test_module).
