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
                           fun history_meck_error_/1,
                           fun shortcut_expect_/1,
                           fun shortcut_expect_negative_arity_/1,
                           fun shortcut_call_return_value_/1,
                           fun shortcut_call_argument_/1,
                           fun delete_/1]]}.

setup() ->
    % Uncomment to run tests with dbg:
    % dbg:tracer(),
    % dbg:p(all, call),
    % dbg:tpl(meck, []),
    ok = meck:new(mymod),
    mymod.

teardown(Module) ->
    catch meck:unload(Module).

%% --- Tests using setup and teardown ------------------------------------------

new_(Mod) ->
    Info = Mod:module_info(),
    ?assert(is_list(Info)).

unload_(Mod) ->
    ok = meck:unload(Mod),
    ?assertEqual(false, code:is_loaded(Mod)).

double_new_(Mod) ->
    ?assertError({already_started, _}, meck:new(Mod)).

validate_(Mod) ->
    ?assertEqual(true, meck:validate(Mod)).

expect_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> ok end),
    ?assertEqual(true, meck:validate(Mod)).

exports_(Mod) ->
    ok = meck:expect(Mod, test1, fun() -> ok end),
    ok = meck:expect(Mod, test2, fun(_) -> ok end),
    ok = meck:expect(Mod, test3, fun(_, _) -> ok end),
    ?assertEqual(0, proplists:get_value(test1, Mod:module_info(exports))),
    ?assertEqual(1, proplists:get_value(test2, Mod:module_info(exports))),
    ?assertEqual(2, proplists:get_value(test3, Mod:module_info(exports))),
    ?assertEqual(true, meck:validate(Mod)).

call_return_value_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> apa end),
    ?assertEqual(apa, Mod:test()),
    ?assertEqual(true, meck:validate(Mod)).

call_argument_(Mod) ->
    ok = meck:expect(Mod, test, fun(hest, 1) -> apa end),
    ?assertEqual(apa, Mod:test(hest, 1)),
    ?assertEqual(true, meck:validate(Mod)).

call_function_clause_(Mod) ->
    ok = meck:expect(Mod, test, fun(hest, 1) -> apa end),
    ?assertError(function_clause, Mod:test(hest, 2)),
    ?assertEqual(false, meck:validate(Mod)).

validate_unexpected_error_(Mod) ->
    ok = meck:expect(Mod, test, fun(hest, 1) -> erlang:error(timeout) end),
    ?assertError(timeout, Mod:test(hest, 1)),
    ?assertEqual(false, meck:validate(Mod)).

validate_expected_error_(Mod) ->
    ok = meck:expect(Mod, test, fun(hest, 1) ->
                                     meck:exception(error, timeout)
                             end),
    ?assertError(timeout, Mod:test(hest, 1)),
    ?assertEqual(true, meck:validate(Mod)).

validate_chained_(Mod) ->
    ok = meck:new(mymod2),
    ok = meck:expect(mymod2, test, fun() ->
                                      meck:exception(error, test_error)
                              end),
    ok = meck:expect(Mod, test, fun() ->
                                     mymod2:test()
                             end),
    ?assertError(test_error, Mod:test()),
    ?assertEqual(false, meck:validate(Mod)),
    ?assertEqual(true, meck:validate(mymod2)),
    ok = meck:unload(mymod2).

stacktrace_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> erlang:error(test_error) end),
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
    ok = meck:expect(Mod, test, fun(1) -> ok end),
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
    ok = meck:expect(Mod, test, fun(hest, 1) -> apa end),
    ?assertError(undef, Mod:test(hest)).

caller_does_not_crash_on_reload_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> timer:sleep(infinity) end),
    Pid = spawn(fun() -> Mod:test() end),
    ok = meck:expect(Mod, new1, fun() -> ok end),
    ok = meck:expect(Mod, new2, fun() -> ok end),
    ok = meck:expect(Mod, new3, fun() -> ok end),
    ?assertEqual(true, is_process_alive(Pid)).

change_func_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> 1 end),
    ?assertEqual(1, Mod:test()),
    MTime = proplists:get_value(time, Mod:module_info(compile)),
    % recompile will result in increased module_info time
    timer:sleep(1100),
    ok = meck:expect(Mod, test, fun() -> 2 end),
    ?assertEqual(2, Mod:test()),
    ?assertEqual(MTime, proplists:get_value(time, Mod:module_info(compile))).

call_original_undef_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> meck:passthrough([]) end),
    ?assertError(undef, Mod:test()).

history_empty_(Mod) ->
    ?assertEqual([], meck:history(Mod)).

history_call_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> ok end),
    ok = meck:expect(Mod, test2, fun(_, _) -> result end),
    Mod:test(),
    Mod:test2(a, b),
    ?assertEqual([{{Mod, test, []}, ok},
                  {{Mod, test2, [a, b]}, result}], meck:history(Mod)).

history_throw_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> throw(test_exception) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, throw, test_exception, _Stacktrace}],
                 meck:history(Mod)).

history_throw_fun_(Mod) ->
    Fun = fun() -> exception_fun end,
    ok = meck:expect(Mod, test, fun() -> throw(Fun) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, throw, Fun, _Stacktrace}],
                 meck:history(Mod)).

history_exit_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> exit(test_exit) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, exit, test_exit, _Stacktrace}],
                 meck:history(Mod)).

history_error_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> erlang:error(test_error) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, error, test_error, _Stacktrace}],
                 meck:history(Mod)).

history_error_args_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> erlang:error(test_error, [fake_args]) end),
    catch Mod:test(),
    History = meck:history(Mod),
    ?assertMatch([{{Mod, test, []}, error, test_error, _Stacktrace}],
                 meck:history(Mod)),
    [{_MFA, error, test_error, Stacktrace}] = History,
    ?assert(lists:any(fun({_M, _F, [fake_args]}) -> true;
                         (_) -> false end, Stacktrace)).

history_meck_throw_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> meck:exception(throw, test_exception) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, throw, test_exception, _Stacktrace}],
                 meck:history(Mod)).

history_meck_throw_fun_(Mod) ->
    Fun = fun() -> exception_fun end,
    ok = meck:expect(Mod, test, fun() -> meck:exception(throw, Fun) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, throw, Fun, _Stacktrace}],
                 meck:history(Mod)).

history_meck_exit_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> meck:exception(exit, test_exit) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, exit, test_exit, _Stacktrace}],
                 meck:history(Mod)).

history_meck_error_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> meck:exception(error, test_error) end),
    catch Mod:test(),
    ?assertMatch([{{Mod, test, []}, error, test_error, _Stacktrace}],
                 meck:history(Mod)).

shortcut_expect_(Mod) ->
    ok = meck:expect(Mod, test, 0, ok),
    ?assertEqual(true, meck:validate(Mod)).

shortcut_expect_negative_arity_(Mod) ->
    ?assertError(function_clause, meck:expect(Mod, test, -1, ok)).

shortcut_call_return_value_(Mod) ->
    ok = meck:expect(Mod, test, 0, apa),
    ?assertEqual(apa, Mod:test()),
    ?assertEqual(true, meck:validate(Mod)).

shortcut_call_argument_(Mod) ->
    ok = meck:expect(Mod, test, fun(hest, 1) -> apa end),
    ?assertEqual(apa, Mod:test(hest, 1)),
    ?assertEqual(true, meck:validate(Mod)).

delete_(Mod) ->
    ok = meck:expect(Mod, test, 2, ok),
    ?assertEqual(ok, meck:delete(Mod, test, 2)),
    ?assertError(undef, Mod:test(a, b)),
    ?assert(meck:validate(Mod)).

%% --- Tests with own setup ----------------------------------------------------

call_original_test() ->
    ?assertEqual({module, meck_test_module}, code:load_file(meck_test_module)),
    ok = meck:new(meck_test_module),
    ?assertEqual({file, ""}, code:is_loaded(meck_test_module_meck_original)),
    ok = meck:expect(meck_test_module, a, fun() -> c end),
    ok = meck:expect(meck_test_module, b, fun() -> meck:passthrough([]) end),
    ?assertEqual(c, meck_test_module:a()),
    ?assertEqual(b, meck_test_module:b()),
    ok = meck:unload(meck_test_module).

unload_renamed_original_test() ->
    ok = meck:new(meck_test_module),
    ok = meck:unload(meck_test_module),
    ?assertEqual(false, code:is_loaded(meck_test_module_meck_original)).

unload_all_test() ->
    Mods = [test_a, test_b, test_c, test_d, test_e],
    ok = meck:new(Mods),
    ?assertEqual(lists:sort(Mods), lists:sort(meck:unload())),
    [?assertEqual(false, code:is_loaded(M)) || M <- Mods].

original_no_file_test() ->
    {ok, Mod, Beam} = compile:forms([{attribute, 1, module, meck_not_on_disk}]),
    {module, Mod} = code:load_binary(Mod, "", Beam),
    ?assertEqual(ok, meck:new(meck_not_on_disk)),
    ok = meck:unload(meck_not_on_disk).

original_has_no_object_code_test() ->
    {ok, Mod, Beam} = compile:forms([{attribute, 1, module, meck_on_disk}]),
    ok = file:write_file("meck_on_disk.beam", Beam),
    {module, Mod} = code:load_binary(Mod, "meck_on_disk.beam", Beam),
    ?assertEqual(ok, meck:new(meck_on_disk)),
    ok = file:delete("meck_on_disk.beam"),
    ok = meck:unload(meck_on_disk).

passthrough_nonexisting_module_test() ->
    ok = meck:new(mymod, [passthrough]),
    ok = meck:expect(mymod, test, fun() -> ok end),
    ?assertEqual(ok, mymod:test()),
    ok = meck:unload(mymod).

passthrough_test() ->
    ok = meck:new(meck_test_module, [passthrough]),
    ok = meck:expect(meck_test_module, a, fun() -> c end),
    ?assertEqual(c, meck_test_module:a()),
    ?assertEqual(b, meck_test_module:b()),
    ?assertEqual({1, 2}, meck_test_module:c(1, 2)),
    ok = meck:unload(meck_test_module).

cover_test() ->
    {ok, _} = cover:compile("../test/meck_test_module.erl"),
    a = meck_test_module:a(),
    b = meck_test_module:b(),
    {1, 2} = meck_test_module:c(1, 2),
    {ok, {meck_test_module, {3,0}}} = cover:analyze(meck_test_module, module),

    ok = meck:new(meck_test_module),
    ok = meck:expect(meck_test_module, a, fun() -> c end),
    ?assertEqual(c, meck_test_module:a()),

    ok = meck:unload(meck_test_module),

    ?assert(not filelib:is_file("meck_test_module.coverdata")),

    {ok, {meck_test_module, {3,0}}} = cover:analyze(meck_test_module, module).

cover_options_test_() ->
    % Test that compilation options (include paths and preprocessor
    % definitions) are used when un-mecking previously cover compiled
    % modules.  Our test module won't compile without compiler options
    % that rebar won't give it, thus the rename dance.
    {setup,
     fun() ->
             ok = file:rename("../test/cover_test_module.dontcompile",
                              "../test/cover_test_module.erl"),
             OldPath = code:get_path(),
             code:add_path("../test"),
             OldPath
     end,
     fun(OldPath) ->
             file:rename("../test/cover_test_module.erl",
                         "../test/cover_test_module.dontcompile"),
             code:set_path(OldPath)
     end,
     ?_test(
        begin
            CompilerOptions = [{i, "../test/include"},
                               {d, 'TEST', true}],
            % The option recover feature depends on having the BEAM
            % file available.
            {ok, _} = compile:file("../test/cover_test_module.erl",
                                   [{outdir, "../test"}|CompilerOptions]),
            {ok, _} = cover:compile("../test/cover_test_module.erl",
                                    CompilerOptions),
            a = cover_test_module:a(),
            b = cover_test_module:b(),
            {1, 2} = cover_test_module:c(1, 2),
            % We get 2 instead of 3 as expected.  Maybe because cover
            % doesn't count include files?
            ?assertEqual({ok, {cover_test_module, {2,0}}},
                         cover:analyze(cover_test_module, module)),

            ok = meck:new(cover_test_module),
            ok = meck:expect(cover_test_module, a, fun() -> c end),
            ?assertEqual(c, cover_test_module:a()),

            ok = meck:unload(cover_test_module),

            ?assert(not filelib:is_file("cover_test_module.coverdata")),

            % 2 instead of 3, as above
            ?assertEqual({ok, {cover_test_module, {2,0}}},
                         cover:analyze(cover_test_module, module))
        end)}.

cover_passthrough_test() ->
    {ok, _} = cover:compile("../test/meck_test_module.erl"),
    {ok, {meck_test_module, {0,3}}} = cover:analyze(meck_test_module, module),

    ok = meck:new(meck_test_module, [passthrough]),
    ok = meck:expect(meck_test_module, a, fun() -> c end),
    ?assertEqual(c, meck_test_module:a()),
    ?assertEqual(b, meck_test_module:b()),
    ?assertEqual({1, 2}, meck_test_module:c(1, 2)),

    ok = meck:unload(meck_test_module),
    {ok, {meck_test_module, {0,3}}} = cover:analyze(meck_test_module, module).

% @doc The mocked module is unloaded if the meck process crashes.
unload_when_crashed_test() ->
    ok = meck:new(mymod),
    ?assertMatch({file, _}, code:is_loaded(mymod)),
    SaltedName = mymod_meck,
    Pid = whereis(SaltedName),
    ?assertEqual(true, is_pid(Pid)),
    unlink(Pid),
    exit(Pid, expected_test_exit),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(SaltedName)),
    ?assertEqual(false, code:is_loaded(mymod)).

% @doc The mocked module is unloaded if the meck process crashes.
unlink_test() ->
    ok = meck:new(mymod, [no_link]),
    SaltedName = mymod_meck,
    {links, Links} = process_info(whereis(SaltedName), links),
    ?assert(not lists:member(self(), Links)),
    ok = meck:unload(mymod).

%% @doc Exception is thrown when you run expect on a non-existing module.
expect_without_new_test() ->
    ?assertError({not_mocked, othermod},
                 meck:expect(othermod, test, fun() -> ok end)).

history_passthrough_test() ->
    ok = meck:new(meck_test_module, [passthrough]),
    ok = meck:expect(meck_test_module, a, fun() -> c end),
    c = meck_test_module:a(),
    b = meck_test_module:b(),
    ?assertEqual([{{meck_test_module, a, []}, c},
                  {{meck_test_module, b, []}, b}],
                 meck:history(meck_test_module)),
    ok = meck:unload(meck_test_module).

multi_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods),
    ok = meck:expect(Mods, test, fun() -> ok end),
    ok = meck:expect(Mods, test2, 0, ok),
    [?assertEqual(ok, M:test()) || M <- Mods],
    ?assert(meck:validate(Mods)),
    ok = meck:unload(Mods).

multi_invalid_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods),
    ok = meck:expect(Mods, test, fun(1) -> ok end),
    ?assertError(function_clause, mod2:test(2)),
    ?assert(not meck:validate(Mods)),
    ok = meck:unload(Mods).

multi_option_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods, [passthrough]),
    ok = meck:expect(Mods, test, fun() -> ok end),
    [?assertEqual(ok, M:test()) || M <- Mods],
    ?assert(meck:validate(Mods)),
    ok = meck:unload(Mods).

multi_shortcut_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods),
    ok = meck:expect(Mods, test, 0, ok),
    [?assertEqual(ok, M:test()) || M <- Mods],
    ?assert(meck:validate(Mods)),
    ok = meck:unload(Mods).

multi_delete_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods),
    ok = meck:expect(Mods, test, 0, ok),
    ?assertEqual(ok, meck:delete(Mods, test, 0)),
    [?assertError(undef, M:test()) || M <- Mods],
    ?assert(meck:validate(Mods)),
    ok = meck:unload(Mods).

handle_cast_unmodified_state_test() ->
    S = dummy_state,
    ?assertEqual({noreply, S}, meck:handle_cast(dummy_msg, S)).

code_change_unmodified_state_test() ->
    S = dummy_state,
    ?assertEqual({ok, S}, meck:code_change(old_version, S, [])).

remote_meck_test_() ->
    {foreach, fun remote_setup/0, fun remote_teardown/1,
     [{with, [T]} || T <- [fun remote_meck_/1,
                           fun remote_meck_cover_/1]]}.

remote_setup() ->
    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),
    Myself = list_to_atom("meck_eunit_test@" ++ Hostname),
    net_kernel:start([Myself, shortnames]),
    {ok, Node} = slave:start_link(list_to_atom(Hostname), meck_remote_test,
                                  "-pa test"),
    {Mod, Bin, File} = code:get_object_code(meck),
    {module, Mod} = rpc:call(Node, code, load_binary, [Mod, File, Bin]),
    {module, meck_test_module} =
        rpc:call(Node, code, load_file, [meck_test_module]),
    {Node, meck_test_module}.

remote_teardown({Node, _Mod}) ->
    ok = slave:stop(Node).

remote_meck_({Node, Mod}) ->
    ?assertEqual(ok, rpc:call(Node, meck, new, [Mod, [no_link]])),
    ?assertEqual(ok, rpc:call(Node, meck, expect, [Mod, test, 0, true])),
    ?assertEqual(true, rpc:call(Node, Mod, test, [])).

remote_meck_cover_({Node, Mod}) ->
    {ok, Mod} = cover:compile(Mod),
    {ok, _Nodes} = cover:start([Node]),
    ?assertEqual(ok, rpc:call(Node, meck, new, [Mod])).

