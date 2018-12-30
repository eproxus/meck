%%=============================================================================
%% Copyright 2010-2017 Adam Lindberg, 2010-2011 Erlang Solutions Ltd
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
%%=============================================================================

-module(meck_tests).

-include_lib("eunit/include/eunit.hrl").

-define(assertTerminated(MonitorRef, Reason, Timeout),
        (fun() ->
                receive
                    {'DOWN', MonitorRef, process, _Pid, Reason} ->
                         ok;
                    {'DOWN', MonitorRef, process, _Pid, AnotherReason} ->
                        erlang:error({dead_for_another_reason, AnotherReason})
                after
                    Timeout ->
                        erlang:error(still_alive)
                end
         end)()).

meck_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun new_/1,
                           fun unload_/1,
                           fun double_new_/1,
                           fun validate_/1,
                           fun expect_/1,
                           fun exports_/1,
                           fun call_return_value_/1,
                           fun call_return_value_improper_list_/1,
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
                           fun history_by_pid_/1,
                           fun reset_/1,
                           fun shortcut_expect_/1,
                           fun shortcut_expect_negative_arity_/1,
                           fun shortcut_call_return_value_/1,
                           fun shortcut_call_argument_/1,
                           fun shortcut_re_add_/1,
                           fun shortcut_opaque_/1,
                           fun shortcut_stacktrace_/1,
                           fun delete_/1,
                           fun expects_/1,
                           fun called_false_no_args_/1,
                           fun called_true_no_args_/1,
                           fun called_true_two_functions_/1,
                           fun called_false_one_arg_/1,
                           fun called_true_one_arg_/1,
                           fun called_false_few_args_/1,
                           fun called_true_few_args_/1,
                           fun called_few_args_matchers_/1,
                           fun called_false_error_/1,
                           fun called_true_error_/1,
                           fun called_with_pid_no_args_/1,
                           fun num_calls_/1,
                           fun num_calls_error_/1,
                           fun num_calls_with_pid_no_args_/1,
                           fun called_wildcard_/1,
                           fun sequence_/1,
                           fun expect_args_sequence_/1,
                           fun expect_arity_sequence_/1,
                           fun expect_complex_sequence_/1,
                           fun sequence_multi_/1,
                           fun loop_/1,
                           fun expect_empty_clause_list_/1,
                           fun expect_args_value_/1,
                           fun expect_args_invalid_call_/1,
                           fun expect_arity_value_/1,
                           fun expect_args_loop_/1,
                           fun expect_arity_loop_/1,
                           fun expect_complex_loop_/1,
                           fun expect_loop_in_seq_/1,
                           fun expect_args_exception_/1,
                           fun expect_arity_exception_/1,
                           fun expect_arity_clause_/1,
                           fun loop_multi_/1,
                           fun expect_args_pattern_override_/1,
                           fun expect_args_pattern_shadow_/1,
                           fun expect_args_pattern_missing_/1,
                           fun expect_args_pattern_invalid_/1,
                           fun expect_args_matchers_/1,
                           fun expect_ret_specs_/1
                          ]]}.

setup() ->
    % Uncomment to run tests with dbg:
    % dbg:tracer(),
    % dbg:p(all, call),
    % dbg:tpl(meck, []),
    ok = meck:new(mymod, [non_strict]),
    mymod.

teardown(Module) ->
    catch meck:unload(Module).

%% --- Tests using setup and teardown -----------------------------------------

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

call_return_value_improper_list_(Mod) ->
    Dict = dict:store(hest, apa, dict:new()),
    ok = meck:expect(Mod, test, 0, Dict),
    ?assertEqual(Dict, Mod:test()),
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
    ok = meck:new(mymod2, [non_strict]),
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
            ?assert(lists:any(fun({M, test, []}) when M == Mod    -> true;
                                 ({M, test, [],[]}) when M == Mod -> true;
                                 (_)                              -> false
                              end, erlang:get_stacktrace()))
    end.

stacktrace_function_clause_(Mod) ->
    ok = meck:expect(Mod, test, fun(1) -> ok end),
    try
        Mod:test(error),
        throw(failed)
    catch
        error:function_clause ->
            Stacktrace = erlang:get_stacktrace(),
            ?assert(lists:any(
                fun ({M, test, [error]}) when M == Mod     -> true;
                    ({M, test, [error], []}) when M == Mod -> true;
                    (_)                                    -> false
                end, Stacktrace))
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
    ok = meck:expect(Mod, test, fun() -> 2 end),
    ?assertEqual(2, Mod:test()).

call_original_undef_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> meck:passthrough([]) end),
    ?assertError(undef, Mod:test()),
    ?assert(not meck:validate(Mod)),
    ?assertEqual(undefined, get('$meck_call')).

history_empty_(Mod) ->
    ?assertEqual([], meck:history(Mod)).

history_call_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> ok end),
    ok = meck:expect(Mod, test2, fun(_, _) -> result end),
    ok = meck:expect(Mod, test3, 0, 3),
    ok = meck:expect(Mod, test4, 0, {1,2,3}),
    Mod:test(),
    Mod:test2(a, b),
    Mod:test3(),
    Mod:test4(),
    ?assertEqual([{self(), {Mod, test,  []},     ok},
                  {self(), {Mod, test2, [a, b]}, result},
                  {self(), {Mod, test3, []},     3},
                  {self(), {Mod, test4, []},     {1,2,3}}], meck:history(Mod)).

history_throw_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> throw(test_exception) end),
    catch Mod:test(),
    ?assertMatch([{_Pid, {Mod, test, []}, throw, test_exception, _Stacktrace}],
                 meck:history(Mod)).

history_throw_fun_(Mod) ->
    Fun = fun() -> exception_fun end,
    ok = meck:expect(Mod, test, fun() -> throw(Fun) end),
    catch Mod:test(),
    ?assertMatch([{_Pid, {Mod, test, []}, throw, Fun, _Stacktrace}],
                 meck:history(Mod)).

history_exit_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> exit(test_exit) end),
    catch Mod:test(),
    ?assertMatch([{_Pid, {Mod, test, []}, exit, test_exit, _Stacktrace}],
                 meck:history(Mod)).

history_error_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> erlang:error(test_error) end),
    catch Mod:test(),
    ?assertMatch([{_Pid, {Mod, test, []}, error, test_error, _Stacktrace}],
                 meck:history(Mod)).

history_error_args_(Mod) ->
    ok = meck:expect(Mod, test,
                     fun() -> erlang:error(test_error, [fake_args]) end),
    catch Mod:test(),
    History = meck:history(Mod),
    ?assertMatch([{_Pid, {Mod, test, []}, error, test_error, _Stacktrace}],
                 meck:history(Mod)),
    [{_Pid, _MFA, error, test_error, Stacktrace}] = History,
    ?assert(lists:any(fun({_M, _F, [fake_args]}) -> true;
                         ({_M, _F, [fake_args], [{file,_},{line,_}]}) -> true;
                         (_) -> false end, Stacktrace)).

history_meck_throw_(Mod) ->
    ok = meck:expect(Mod, test,
                     fun() -> meck:exception(throw, test_exception) end),
    catch Mod:test(),
    ?assertMatch([{_Pid, {Mod, test, []}, throw, test_exception, _Stacktrace}],
                 meck:history(Mod)).

history_meck_throw_fun_(Mod) ->
    Fun = fun() -> exception_fun end,
    ok = meck:expect(Mod, test, fun() -> meck:exception(throw, Fun) end),
    catch Mod:test(),
    ?assertMatch([{_Pid, {Mod, test, []}, throw, Fun, _Stacktrace}],
                 meck:history(Mod)).

history_meck_exit_(Mod) ->
    ok = meck:expect(Mod, test, fun() -> meck:exception(exit, test_exit) end),
    catch Mod:test(),
    ?assertMatch([{_Pid, {Mod, test, []}, exit, test_exit, _Stacktrace}],
                 meck:history(Mod)).

history_meck_error_(Mod) ->
    ok = meck:expect(Mod, test,
                     fun() -> meck:exception(error, test_error) end),
    catch Mod:test(),
    ?assertMatch([{_Pid, {Mod, test, []}, error, test_error, _Stacktrace}],
                 meck:history(Mod)).

history_by_pid_(Mod) ->
    ok = meck:expect(Mod, test1, fun() -> ok end),
    ok = meck:expect(Mod, test2, fun() -> ok end),

    TestPid = self(),
    Fun = fun() ->
                  Mod:test1(),
                  TestPid ! {self(), done}
          end,
    Pid = spawn(Fun),
    Mod:test1(),
    Mod:test2(),
    receive {Pid, done} -> ok end,
    ?assertEqual([{Pid, {Mod, test1, []}, ok}], meck:history(Mod, Pid)),
    ?assertEqual([{TestPid, {Mod, test1, []}, ok},
                  {TestPid, {Mod, test2, []}, ok}],
                 meck:history(Mod, TestPid)),
    ?assertEqual(meck:history(Mod), meck:history(Mod, '_')).

reset_(Mod) ->
    % Given
    meck:expect(Mod, test1, fun() -> ok end),
    meck:expect(Mod, test2, fun() -> ok end),
    Mod:test1(),
    Mod:test2(),
    % When
    meck:reset(Mod),
    Mod:test1(),
    % Then
    ?assertMatch([{_Pid, {Mod, test1, []}, ok}], meck:history(Mod)).

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
    ok = meck:expect(Mod, test, 2, apa),
    ?assertEqual(apa, Mod:test(hest, 1)),
    ?assertEqual(true, meck:validate(Mod)).

shortcut_re_add_(Mod) ->
    ok = meck:expect(Mod, test, 2, apa),
    ?assertEqual(apa, Mod:test(hest, 1)),
    ok = meck:expect(Mod, test, 2, new),
    ?assertEqual(new, Mod:test(hest, 1)),
    ?assertEqual(true, meck:validate(Mod)).

shortcut_opaque_(Mod) ->
    Ref = make_ref(),
    ok = meck:expect(Mod, test, 0, {test, [a, self()], Ref}),
    ?assertMatch({test, [a, P], Ref} when P == self(), Mod:test()).

shortcut_stacktrace_(Mod) ->
    ok = meck:expect(Mod, test, [true], ok),
    ?assertEqual(
        {'EXIT', {function_clause, [{mymod, test, [false], []}]}},
        catch(Mod:test(false))
    ).

delete_(Mod) ->
    ok = meck:expect(Mod, test, 2, ok),
    ?assertEqual(ok, meck:delete(Mod, test, 2)),
    ?assertError(undef, Mod:test(a, b)),
    ?assert(meck:validate(Mod)).

expects_(Mod) ->
    ?assertEqual([], meck:expects(Mod)),
    ok = meck:expect(Mod, test, 2, ok),
    ?assertEqual([{Mod, test, 2}], meck:expects(Mod)),
    ok = meck:expect(Mod, test2, 0, ok),
    ?assertEqual([{Mod, test, 2}, {Mod, test2, 0}], lists:sort(meck:expects(Mod))).

called_false_no_args_(Mod) ->
    Args = [],
    ok = meck:expect(Mod, test, length(Args), ok),
    assert_called(Mod, test, Args, false),
    ok.

called_true_no_args_(Mod) ->
    Args = [],
    ok = meck:expect(Mod, test, length(Args), ok),
    ok = apply(Mod, test, Args),
    assert_called(Mod, test, Args, true),
    ok.

called_true_two_functions_(Mod) ->
    Args = [],
    ok = meck:expect(Mod, test1, length(Args), ok),
    ok = meck:expect(Mod, test2, length(Args), ok),
    ok = apply(Mod, test1, Args),
    ok = apply(Mod, test2, Args),
    assert_called(Mod, test2, Args, true),
    ok.

called_false_one_arg_(Mod) ->
    Args = ["hello"],
    ok = meck:expect(Mod, test, length(Args), ok),
    assert_called(Mod, test, Args, false),
    ok.

called_true_one_arg_(Mod) ->
    Args = ["hello"],
    ok = meck:expect(Mod, test, length(Args), ok),
    ok = apply(Mod, test, Args),
    assert_called(Mod, test, Args, true),
    ok.

called_false_few_args_(Mod) ->
    Args = [one, 2, {three, 3}, "four"],
    ok = meck:expect(Mod, test, length(Args), ok),
    assert_called(Mod, test, Args, false),
    ok.

called_true_few_args_(Mod) ->
    Args = [one, 2, {three, 3}, "four"],
    ok = meck:expect(Mod, test, length(Args), ok),
    ok = apply(Mod, test, Args),
    assert_called(Mod, test, Args, true),
    ok.

called_few_args_matchers_(Mod) ->
    Args = [one, 2, {three, 3}, "four"],
    ok = meck:expect(Mod, test, length(Args), ok),
    ok = apply(Mod, test, Args),
    assert_called(Mod, test, ['_', meck:is(fun(X) -> X == 2 end), {'_', 3}, "four"], true),
    assert_called(Mod, test, ['_', meck:is(fun(X) -> X == 3 end), {'_', 3}, "four"], false),
    ok.

called_false_error_(Mod) ->
    Args = [one, "two", {3, 3}],
    TestFun = fun (_, _, _) -> meck:exception(error, my_error) end,
    ok = meck:expect(Mod, test, TestFun),
    assert_called(Mod, test, Args, false),
    ok.

called_true_error_(Mod) ->
    Args = [one, "two", {3, 3}],
    expect_catch_apply(Mod, test, Args),
    assert_called(Mod, test, Args, true),
    ok.

called_with_pid_no_args_(Mod) ->
    Args = [],
    ok = meck:expect(Mod, test, length(Args), ok),
    Pid = spawn_caller_and_sync(Mod, test, Args),
    assert_called(Mod, test, Args, self(), false),
    assert_called(Mod, test, Args, Pid, true),
    ok = apply(Mod, test, Args),
    assert_called(Mod, test, Args, self(), true),
    ?assertEqual(meck:called(Mod, test, Args, '_'),
                 meck:called(Mod, test, Args)).

spawn_caller_and_sync(Mod, Func, Args) ->
    TestPid = self(),
    Fun = fun() ->
                  catch apply(Mod, Func, Args),
                  TestPid ! {self(), done}
          end,
    Pid = spawn(Fun),
    receive {Pid, done} -> ok end, % sync with the spawned process
    Pid.

num_calls_(Mod) ->
    Args = [],
    IncorrectArgs = [foo],
    ok = meck:expect(Mod, test1, length(Args), ok),
    ?assertEqual(0, meck:num_calls(Mod, test1, Args)),
    ok = apply(Mod, test1, Args),
    ?assertEqual(1, meck:num_calls(Mod, test1, Args)),
    ?assertEqual(0, meck:num_calls(Mod, test1, IncorrectArgs)).

num_calls_error_(Mod) ->
    Args = [one, "two", {3, 3}],
    expect_catch_apply(Mod, test, Args),
    ?assertEqual(1, meck:num_calls(Mod, test, Args)).

num_calls_with_pid_no_args_(Mod) ->
    Args = [],
    ok = meck:expect(Mod, test, length(Args), ok),
    Pid = spawn_caller_and_sync(Mod, test, Args),
    ?assertEqual(0, meck:num_calls(Mod, test, Args, self())),
    ?assertEqual(1, meck:num_calls(Mod, test, Args, Pid)),
    ok = apply(Mod, test, Args),
    ?assertEqual(1, meck:num_calls(Mod, test, Args, self())),
    ?assertEqual(meck:num_calls(Mod, test, Args, '_'),
                 meck:num_calls(Mod, test, Args)).

expect_catch_apply(Mod, Func, Args) ->
    TestFun = fun (_, _, _) -> meck:exception(error, my_error) end,
    ok = meck:expect(Mod, Func, TestFun),
    catch apply(Mod, Func, Args).

called_wildcard_(Mod) ->
    Args = [one, 2, {three, 3}, "four"],
    ok = meck:expect(Mod, test, length(Args), ok),
    ok = apply(Mod, test, Args),
    assert_called(Mod, test, [one, '_', {three, '_'}, "four"], true),
    ok.

sequence_(Mod) ->
    Sequence = [a, b, c, d, e],
    ?assertEqual(ok, meck:sequence(Mod, s, 2, Sequence)),
    ?assertEqual(Sequence,
                 [Mod:s(a, b) || _ <- lists:seq(1, length(Sequence))]),
    ?assertEqual([e, e, e, e, e],
                 [Mod:s(a, b) || _ <- lists:seq(1, 5)]),
    ?assert(meck:validate(Mod)).

sequence_multi_(Mod) ->
    meck:new(mymod2, [non_strict]),
    Mods = [Mod, mymod2],
    Sequence = [a, b, c, d, e],
    ?assertEqual(ok, meck:sequence(Mods, s, 2, Sequence)),
    ?assertEqual(Sequence,
                 [Mod:s(a, b) || _ <- lists:seq(1, length(Sequence))]),
    ?assertEqual([e, e, e, e, e],
                 [Mod:s(a, b) || _ <- lists:seq(1, 5)]),
    ?assertEqual(Sequence,
                 [mymod2:s(a, b) || _ <- lists:seq(1, length(Sequence))]),
    ?assertEqual([e, e, e, e, e],
                 [mymod2:s(a, b) || _ <- lists:seq(1, 5)]),
    ?assert(meck:validate(Mods)).

expect_empty_clause_list_(Mod) ->
    ?assertError(empty_clause_list, meck:expect(Mod, dummy, [])).

expect_args_value_(Mod) ->
    %% When
    meck:expect(Mod, val, [1001], meck:val(a)),
    %% Then
    ?assertEqual(a, Mod:val(1001)),
    ?assertEqual(a, Mod:val(1001)).

expect_args_invalid_call_(Mod) ->
    %% When
    meck:expect(Mod, val, [1001], meck:val(a)),
    %% Then
    ?assertError(function_clause, Mod:val(1002)).

expect_arity_value_(Mod) ->
    %% When
    meck:expect(Mod, val, 1, meck:val(a)),
    %% Then
    ?assertEqual(a, Mod:val(1001)),
    ?assertEqual(a, Mod:val(1001)).

expect_args_sequence_(Mod) ->
    %% When
    meck:expect(Mod, seq, [1001], meck:seq([a, b, c])),
    %% Then
    ?assertEqual(a, Mod:seq(1001)),
    ?assertEqual(b, Mod:seq(1001)),
    ?assertEqual(c, Mod:seq(1001)),
    ?assertEqual(c, Mod:seq(1001)),
    ?assertEqual(c, Mod:seq(1001)).

expect_arity_sequence_(Mod) ->
    %% When
    meck:expect(Mod, seq, 1, meck:seq([a, b, c])),
    %% Then
    ?assertEqual(a, Mod:seq(1001)),
    ?assertEqual(b, Mod:seq(1001)),
    ?assertEqual(c, Mod:seq(1001)),
    ?assertEqual(c, Mod:seq(1001)),
    ?assertEqual(c, Mod:seq(1001)).

expect_complex_sequence_(Mod) ->
    %% When
    meck:expect(Mod, seq, 1, meck:seq([meck:val(a),
                                       meck:seq([b, c]),
                                       meck:seq([meck:raise(error, d),
                                                 meck:seq([e, f, g]),
                                                 h,
                                                 meck:val(i)])])),
    %% Then
    ?assertEqual(a, Mod:seq(1001)),
    ?assertEqual(b, Mod:seq(1001)),
    ?assertEqual(c, Mod:seq(1001)),
    ?assertException(error, d, Mod:seq(1001)),
    ?assertEqual(e, Mod:seq(1001)),
    ?assertEqual(f, Mod:seq(1001)),
    ?assertEqual(g, Mod:seq(1001)),
    ?assertEqual(h, Mod:seq(1001)),
    ?assertEqual(i, Mod:seq(1001)),
    ?assertEqual(i, Mod:seq(1001)),
    ?assertEqual(i, Mod:seq(1001)).

loop_(Mod) ->
    Loop = [a, b, c, d, e],
    ?assertEqual(ok, meck:loop(Mod, l, 2, Loop)),
    [?assertEqual(V, Mod:l(a, b)) || _ <- lists:seq(1, length(Loop)), V <- Loop],
    ?assert(meck:validate(Mod)).

expect_args_loop_(Mod) ->
    %% When
    meck:expect(Mod, loop, [1001], meck:loop([a, b, c])),
    %% Then
    ?assertEqual(a, Mod:loop(1001)),
    ?assertEqual(b, Mod:loop(1001)),
    ?assertEqual(c, Mod:loop(1001)),
    ?assertEqual(a, Mod:loop(1001)),
    ?assertEqual(b, Mod:loop(1001)),
    ?assertEqual(c, Mod:loop(1001)),
    ?assertEqual(a, Mod:loop(1001)).

expect_arity_loop_(Mod) ->
    %% When
    meck:expect(Mod, loop, 1, meck:loop([a, b, c])),
    %% Then
    ?assertEqual(a, Mod:loop(1001)),
    ?assertEqual(b, Mod:loop(1001)),
    ?assertEqual(c, Mod:loop(1001)),
    ?assertEqual(a, Mod:loop(1001)),
    ?assertEqual(b, Mod:loop(1001)),
    ?assertEqual(c, Mod:loop(1001)),
    ?assertEqual(a, Mod:loop(1001)).

expect_complex_loop_(Mod) ->
    %% When
    meck:expect(Mod, loop, 1, meck:loop([meck:val(a),
                                         meck:seq([b, c]),
                                         meck:seq([meck:raise(error, d),
                                                   meck:seq([e, f, g]),
                                                   h,
                                                   meck:val(i)])])),
    %% Then
    ?assertEqual(a, Mod:loop(1001)),
    ?assertEqual(b, Mod:loop(1001)),
    ?assertEqual(c, Mod:loop(1001)),
    ?assertException(error, d, Mod:loop(1001)),
    ?assertEqual(e, Mod:loop(1001)),
    ?assertEqual(f, Mod:loop(1001)),
    ?assertEqual(g, Mod:loop(1001)),
    ?assertEqual(h, Mod:loop(1001)),
    ?assertEqual(i, Mod:loop(1001)),
    %% The second round
    ?assertEqual(a, Mod:loop(1001)),
    ?assertEqual(b, Mod:loop(1001)),
    ?assertEqual(c, Mod:loop(1001)),
    ?assertException(error, d, Mod:loop(1001)),
    ?assertEqual(e, Mod:loop(1001)),
    ?assertEqual(f, Mod:loop(1001)),
    ?assertEqual(g, Mod:loop(1001)),
    ?assertEqual(h, Mod:loop(1001)),
    ?assertEqual(i, Mod:loop(1001)),
    %% The third round
    ?assertEqual(a, Mod:loop(1001)).

expect_loop_in_seq_(Mod) ->
    %% When
    meck:expect(Mod, seq, 1, meck:seq([meck:val(a),
                                       meck:loop([b,
                                                  meck:raise(throw, c),
                                                  d]),
                                       meck:val(e), % Never returned
                                       meck:raise(exit, f)])),
    %% Then
    ?assertEqual(a, Mod:seq(1001)),
    ?assertEqual(b, Mod:seq(1001)),
    ?assertException(throw, c, Mod:seq(1001)),
    ?assertEqual(d, Mod:seq(1001)),
    %% The second round
    ?assertEqual(b, Mod:seq(1001)),
    ?assertException(throw, c, Mod:seq(1001)),
    ?assertEqual(d, Mod:seq(1001)),
    %% The third round
    ?assertEqual(b, Mod:seq(1001)).

expect_args_exception_(Mod) ->
    %% Given
    meck:expect(Mod, f, [{[1001], meck:raise(error, a)},
                         {[1002], meck:raise(throw, b)},
                         {[1003], meck:raise(exit, c)},
                         {[1004], meck:val(d)}]),
    %% When/Then
    ?assertException(error, a, Mod:f(1001)),
    ?assertException(throw, b, Mod:f(1002)),
    ?assertException(exit, c, Mod:f(1003)),
    ?assertMatch(d, Mod:f(1004)).

expect_arity_exception_(Mod) ->
    %% Given
    meck:expect(Mod, f, 1, meck:raise(error, a)),
    %% When/Then
    ?assertError(a, Mod:f(1001)).

expect_arity_clause_(Mod) ->
    %% Given
    meck:expect(Mod, foo, [{2, blah}]),
    %% When/Then
    ?assertMatch(blah, Mod:foo(1, 2)),
    ?assertError(_, Mod:foo(1, 2, 3)).

loop_multi_(Mod) ->
    meck:new(mymod2, [non_strict]),
    Mods = [Mod, mymod2],
    Loop = [a, b, c, d, e],
    ?assertEqual(ok, meck:loop(Mods, l, 2, Loop)),
    [[?assertEqual(V, M:l(a, b)) || _ <- lists:seq(1, length(Loop)), V <- Loop]
     || M <- Mods],
    ?assert(meck:validate(Mods)).

expect_args_pattern_override_(Mod) ->
    %% When
    meck:expect(Mod, f, [{[1, 1],     a},
                         {[1, '_'],   b},
                         {['_', '_'], c}]),
    %% Then
    ?assertEqual(a, Mod:f(1, 1)),
    ?assertEqual(b, Mod:f(1, 2)),
    ?assertEqual(c, Mod:f(2, 2)).

expect_args_pattern_shadow_(Mod) ->
    %% When
    meck:expect(Mod, f, [{[1, 1],     a},
                         {['_', '_'], c},
                         {[1, '_'],   b}]),
    %% Then
    ?assertEqual(a, Mod:f(1, 1)),
    ?assertEqual(c, Mod:f(1, 2)),
    ?assertEqual(c, Mod:f(2, 2)).

expect_args_pattern_missing_(Mod) ->
    %% When
    meck:expect(Mod, f, [{[1, 1],   a},
                         {[1, '_'], b}]),
    %% Then
    ?assertError(function_clause, Mod:f(2, 2)),
    ?assertEqual(a, Mod:f(1, 1)),
    ?assertEqual(b, Mod:f(1, 2)).

expect_args_pattern_invalid_(Mod) ->
    %% When/Then
    ?assertError({invalid_arity, {{expected, 2},
                                  {actual, 3},
                                  {clause, {[1, 2, 3], b}}}},
                 meck:expect(Mod, f, [{[1, 2],    a},
                                      {[1, 2, 3], b}])).

expect_args_matchers_(Mod) ->
    %% When
    meck:expect(Mod, f, [{[1, meck:is(fun(X) -> X == 1 end)], a},
                         {[1, meck:is(fun(X) -> X < 3 end)],  b},
                         {['_', '_'],                         c}]),
    %% Then
    ?assertEqual(a, Mod:f(1, 1)),
    ?assertEqual(b, Mod:f(1, 2)),
    ?assertEqual(c, Mod:f(2, 2)).

expect_ret_specs_(Mod) ->
    %% When
    meck:expect(Mod, f, [{[1, 1],     meck:seq([a, b, c])},
                         {[1, '_'],   meck:loop([d, e])},
                         {['_', '_'], meck:val(f)}]),
    %% Then
    ?assertEqual(d, Mod:f(1, 2)),
    ?assertEqual(f, Mod:f(2, 2)),
    ?assertEqual(e, Mod:f(1, 2)),
    ?assertEqual(a, Mod:f(1, 1)),
    ?assertEqual(d, Mod:f(1, 2)),
    ?assertEqual(b, Mod:f(1, 1)),
    ?assertEqual(c, Mod:f(1, 1)),
    ?assertEqual(f, Mod:f(2, 2)),
    ?assertEqual(c, Mod:f(1, 1)),
    ?assertEqual(e, Mod:f(1, 2)),
    ?assertEqual(c, Mod:f(1, 1)).

%% --- Tests with own setup ----------------------------------------------------

merge_expects_module_test() ->
    Mod = merge_mod,
    meck:new(Mod, [non_strict, merge_expects]),
    %% Given
    meck:expect(Mod, f, [2001], meck:raise(error, a)),
    meck:expect(Mod, f, [2002], meck:raise(throw, b)),
    meck:expect(Mod, f, [2003], meck:raise(exit, c)),
    meck:expect(Mod, f, [2004], meck:val(d)),
    %% When/Then
    ?assertException(error, a, Mod:f(2001)),
    ?assertException(throw, b, Mod:f(2002)),
    ?assertException(exit, c, Mod:f(2003)),
    ?assertMatch(d, Mod:f(2004)),
    meck:unload(Mod).

merge_expects_ret_specs_test() ->
    Mod = merge_mod,
    meck:new(Mod, [non_strict, merge_expects]),
    %% When
    meck:expect(Mod, f, [1, 1],   meck:seq([a, b, c])),
    meck:expect(Mod, f, [1, '_'], meck:loop([d, e])),
    meck:expect(Mod, f, ['_', '_'], meck:val(f)),
    %% Then
    ?assertEqual(d, Mod:f(1, 2)),
    ?assertEqual(f, Mod:f(2, 2)),
    ?assertEqual(e, Mod:f(1, 2)),
    ?assertEqual(a, Mod:f(1, 1)),
    ?assertEqual(d, Mod:f(1, 2)),
    ?assertEqual(b, Mod:f(1, 1)),
    ?assertEqual(c, Mod:f(1, 1)),
    ?assertEqual(f, Mod:f(2, 2)),
    ?assertEqual(c, Mod:f(1, 1)),
    ?assertEqual(e, Mod:f(1, 2)),
    ?assertEqual(c, Mod:f(1, 1)),
    meck:unload(Mod).

undefined_module_test() ->
    %% When/Then
    ?assertError({{undefined_module, blah}, _}, meck:new(blah, [no_link])).

undefined_function_test() ->
    %% Given
    meck:new(meck_test_module),
    %% When/Then
    meck:expect(meck_test_module, b, 0, ok),
    ?assertError({undefined_function, {meck_test_module, b, 1}},
                 meck:expect(meck_test_module, b, 1, ok)),
    meck:unload(meck_test_module).

call_original_test() ->
    false = code:purge(meck_test_module),
    ?assertEqual({module, meck_test_module}, code:load_file(meck_test_module)),
    ok = meck:new(meck_test_module, [no_passthrough_cover]),
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
    ok = meck:new(Mods, [non_strict]),
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

passthrough_with_no_object_code_test() ->
    {ok, Mod, Beam} = compile:forms([{attribute, 1, module, no_abstract_code}]),
    ok = file:write_file("no_abstract_code.beam", Beam),
    {module, Mod} = code:load_binary(Mod, "no_abstract_code.beam", Beam),
    ?assertError(
        {abstract_code_not_found, Mod},
        meck:new(no_abstract_code, [passthrough, no_link])
    ),
    ok = file:delete("no_abstract_code.beam").

passthrough_nonexisting_module_test() ->
    ok = meck:new(mymod, [passthrough, non_strict]),
    ok = meck:expect(mymod, test, fun() -> ok end),
    ?assertEqual(ok, mymod:test()),
    ok = meck:unload(mymod).

passthrough_test() ->
    passthrough_test([]).

passthrough_test(Opts) ->
    ok = meck:new(meck_test_module, [passthrough|Opts]),
    ok = meck:expect(meck_test_module, a, fun() -> c end),
    ?assertEqual(c, meck_test_module:a()),
    ?assertEqual(b, meck_test_module:b()),
    ?assertEqual({1, 2}, meck_test_module:c(1, 2)),
    ok = meck:unload(meck_test_module).

passthrough_different_arg_test() ->
    ok = meck:new(meck_test_module),
    ok = meck:expect(meck_test_module, c,
                     fun(_, _) -> meck:passthrough([x, y]) end),
    ?assertEqual({x, y}, meck_test_module:c(1, 2)),
    ok = meck:unload(meck_test_module).

passthrough_bif_test() ->
    ?assertEqual(ok, meck:new(file, [unstick, passthrough])),
    ?assertEqual(ok, meck:unload(file)).

stub_all_test() ->
    ok = meck:new(meck_test_module, [{stub_all, meck:seq([a, b])}]),
    ok = meck:expect(meck_test_module, a, [], c),
    ?assertEqual(c, meck_test_module:a()),
    ?assertEqual(a, meck_test_module:b()),
    ?assertEqual(b, meck_test_module:b()),
    ?assertEqual(b, meck_test_module:b()),
    ?assertEqual(a, meck_test_module:c(1, 2)),
    ?assertEqual(b, meck_test_module:c(1, 2)),
    ?assertEqual(b, meck_test_module:c(1, 2)),
    ok = meck:unload(meck_test_module).

stub_all_default_test() ->
    ok = meck:new(meck_test_module, [stub_all]),
    ?assertEqual(ok, meck_test_module:c(1, 2)),
    ok = meck:unload(meck_test_module).

stub_all_undefined_test() ->
    ok = meck:new(meck_test_module, [{stub_all, undefined}]),
    ?assertEqual(undefined, meck_test_module:c(1, 2)),
    ok = meck:unload(meck_test_module).

stub_all_true_test() ->
    ok = meck:new(meck_test_module, [{stub_all, true}]),
    ?assertEqual(true, meck_test_module:c(1, 2)),
    ok = meck:unload(meck_test_module).

stub_all_overridden_by_passthrough_test() ->
    ok = meck:new(meck_test_module, [stub_all, passthrough]),
    ?assertEqual(a, meck_test_module:a()),
    ok = meck:unload(meck_test_module).

mock_file_existing_test() ->
    %% Given
    ExistingFile = test_file(?MODULE, ".erl"),
    {ok, ExistsInfo} = file:read_file_info(ExistingFile),
    meck:new(file, [unstick, passthrough]),
    %% When
    meck:expect(file, read_file_info, fun(Path) -> meck:passthrough([Path]) end),
    %% Then
    ?assertEqual({ok, ExistsInfo}, file:read_file_info(ExistingFile)),
    %% Cleanup
    meck:unload(file).

mock_file_missing_test() ->
    %% Given
    MissingFile = "blah.erl",
    {error, enoent} = file:read_file_info(MissingFile),
    meck:new(file, [unstick, passthrough]),
    %% When
    meck:expect(file, read_file_info, 1, {ok, no_info}),
    %% Then
    ?assertEqual({ok, no_info}, file:read_file_info(MissingFile)),
    %% Cleanup
    meck:unload(file).

cover_test() ->
    {ok, _} = cover:compile(test_file(meck_test_module, ".erl")),
    a = meck_test_module:a(),
    b = meck_test_module:b(),
    {1, 2} = meck_test_module:c(1, 2),
    {ok, {meck_test_module, {3,0}}} = cover:analyze(meck_test_module, module),
    run_mock_no_cover_file(meck_test_module),
    {ok, {meck_test_module, {3,0}}} = cover:analyze(meck_test_module, module).

cover_options_test_() ->
    {foreach, fun compile_options_setup/0, fun compile_options_teardown/1,
     [{with, [T]} || T <- [fun cover_options_/1,
                           fun cover_options_fail_/1
                          ]]}.

compile_options_setup() ->
    Module = cover_test_module,
    % Our test module won't compile without compiler options that
    % rebar won't give it, thus the rename dance.
    Src = test_file(Module, ".erl"),
    ok = file:rename(test_file(Module, ".dontcompile"), Src),
    OldPath = code:get_path(),
    code:add_path(test_dir()),
    {OldPath, Src, Module}.

compile_options_teardown({OldPath, Src, Module}) ->
    file:rename(Src, test_file(Module, ".dontcompile")),
    code:purge(Module),
    code:delete(Module),
    code:set_path(OldPath).

cover_options_({_OldPath, Src, Module}) ->
    % Test that compilation options (include paths and preprocessor
    % definitions) are used when un-mecking previously cover compiled
    % modules.
    CompilerOptions = [{i, test_include()}, {d, 'TEST', true}],
    % The option recover feature depends on having the BEAM file
    % available.
    {ok, _} = compile:file(Src, [{outdir, test_dir()}|CompilerOptions]),
    {ok, _} = cover:compile(Src, CompilerOptions),
    a      = Module:a(),
    b      = Module:b(),
    {1, 2} = Module:c(1, 2),
    % We get 2 instead of 3 as expected.  Maybe because cover doesn't
    % count include files?
    ?assertEqual({ok, {Module, {2,0}}}, cover:analyze(Module, module)),
    run_mock_no_cover_file(Module),
    % 2 instead of 3, as above
    ?assertEqual({ok, {Module, {2,0}}}, cover:analyze(Module, module)).

cover_options_fail_({_OldPath, Src, Module}) ->
    %% This may look like the test above but there is a subtle
    %% difference.  When `cover:compile_beam' is called it squashes
    %% compile options.  This test verifies that function `b/0', which
    %% relies on the `TEST' directive being set can still be called
    %% after the module is meck'ed.
    CompilerOptions = [
        debug_info,
        {i, test_include()},
        {outdir, test_dir()},
        {d, 'TEST', true}
    ],
    {ok, _} = compile:file(Src, CompilerOptions),
    ?assertEqual(
        proplists:delete(outdir, lists:sort(CompilerOptions)),
        proplists:delete(outdir, lists:sort(meck_code:compile_options(Module)))
    ),
    {ok, _} = cover:compile_beam(Module),
    ?assertEqual(
        [{i, test_include()}, {d, 'TEST', true}],
        meck_code:compile_options(Module)
    ),
    a      = Module:a(),
    b      = Module:b(),
    {1, 2} = Module:c(1, 2),
    ?assertEqual({ok, {Module, {2,0}}}, cover:analyze(Module, module)),
    ok = meck:new(Module, [passthrough]),
    ok = meck:expect(Module, a, fun () -> c end),
    ?assertEqual(c, Module:a()),
    ?assertEqual(b, Module:b()),
    ?assertEqual({1, 2}, Module:c(1, 2)),
    ok = meck:unload(Module),
    %% Verify passthru calls went to cover
    ?assertEqual({ok, {Module, 4}}, cover:analyze(Module, calls, module)).

test_file(Module, Ext) ->
    filename:join(test_dir(), atom_to_list(Module) ++ Ext).

test_dir() ->
    case code:which(?MODULE) of
        Filename when is_list(Filename) ->
            filename:dirname(Filename);
        Atom when is_atom(Atom) ->
            error({test_dir_not_found, ?MODULE, Atom})
    end.

test_include() -> filename:join(test_dir(), "include").

run_mock_no_cover_file(Module) ->
    ok = meck:new(Module),
    ok = meck:expect(Module, a, fun () -> c end),
    ?assertEqual(c, Module:a()),
    ok = meck:unload(Module),
    ?assert(not filelib:is_file(atom_to_list(Module) ++ ".coverdata")).

%% @doc Verify that passthrough calls _don't_ appear in cover
%% analysis.
no_cover_passthrough_test() ->
    {ok, _} = cover:compile("test/meck_test_module.erl"),
    {ok, {meck_test_module, {0,3}}} = cover:analyze(meck_test_module, module),
    passthrough_test([no_passthrough_cover]),
    {ok, {meck_test_module, {0,3}}} = cover:analyze(meck_test_module, module).

%% @doc Verify that passthrough calls appear in cover analysis.
cover_passthrough_test() ->
    {ok, _} = cover:compile("test/meck_test_module.erl"),
    ?assertEqual({ok, {meck_test_module, {0,3}}},
                 cover:analyze(meck_test_module, module)),
    passthrough_test([]),
    ?assertEqual({ok, {meck_test_module, {2,1}}},
                 cover:analyze(meck_test_module, module)).

cover_path_test() ->
    {ok, _} = cover:compile("test/meck_test_module.erl"),
    ?assertEqual({ok, {meck_test_module, {0,3}}},
                 cover:analyze(meck_test_module, module)),
    ok = meck:new(meck_test_module, [passthrough]),
    ok = meck:expect(meck_test_module, a, fun() -> c end),
    ?assertEqual(c, meck_test_module:a()),
    ?assertEqual(b, meck_test_module:b()),
    ?assertEqual({1, 2}, meck_test_module:c(1, 2)),
    {ok, CWD} = file:get_cwd(),
    try
        ok = file:set_cwd("/tmp"),
        ok = meck:unload(meck_test_module),
        ?assertEqual({ok, {meck_test_module, {2,1}}},
                     cover:analyze(meck_test_module, module))
    after
        ok = file:set_cwd(CWD)
    end.

% @doc The mocked module is unloaded if the meck process crashes.
unload_when_crashed_test() ->
    ok = meck:new(mymod, [non_strict]),
    ?assertMatch({file, _}, code:is_loaded(mymod)),
    SaltedName = mymod_meck,
    Pid = whereis(SaltedName),
    ?assertEqual(true, is_pid(Pid)),
    unlink(Pid),
    error_logger:tty(false),
    exit(Pid, expected_test_exit),
    timer:sleep(100),
    error_logger:tty(true),
    ?assertEqual(undefined, whereis(SaltedName)),
    ?assertEqual(false, code:is_loaded(mymod)).

% @doc The mocked module is unloaded if the meck process crashes.
unlink_test() ->
    ok = meck:new(mymod, [no_link, non_strict]),
    SaltedName = mymod_meck,
    {links, Links} = process_info(whereis(SaltedName), links),
    ?assert(not lists:member(self(), Links)),
    ok = meck:unload(mymod).

%% @doc A concurrent process calling into the mocked module while it's
%% being unloaded gets either the mocked response or the original
%% response, but won't crash.
atomic_unload_test() ->
    ok = meck:new(meck_test_module),
    ok = meck:expect(meck_test_module, a, fun () -> c end),

    %% Suspend the meck_proc in order to ensure all messages are in
    %% its inbox in the correct order before it would process them
    Proc = meck_util:proc_name(meck_test_module),
    sys:suspend(Proc),
    StopReq = concurrent_req(
                Proc,
                fun () -> ?assertEqual(ok, meck:unload(meck_test_module)) end),
    SpecReq = concurrent_req(
                Proc,
                fun () -> ?assertMatch(V when V =:= a orelse V =:= c,
                                       meck_test_module:a())
                end),
    sys:resume(Proc),

    ?assertEqual(normal, wait_concurrent_req(StopReq)),
    ?assertEqual(normal, wait_concurrent_req(SpecReq)).

%% @doc Exception is thrown when you run expect on a non-existing (and not yet
%% mocked) module.
expect_without_new_test() ->
    ?assertError({not_mocked, othermod},
                 meck:expect(othermod, test, fun() -> ok end)).

history_passthrough_test() ->
    ok = meck:new(meck_test_module, [passthrough]),
    ok = meck:expect(meck_test_module, a, fun() -> c end),
    c = meck_test_module:a(),
    b = meck_test_module:b(),
    ?assertEqual([{self(), {meck_test_module, a, []}, c},
                  {self(), {meck_test_module, b, []}, b}],
                 meck:history(meck_test_module)),
    ok = meck:unload(meck_test_module).

multi_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods, [non_strict]),
    ok = meck:expect(Mods, test, fun() -> ok end),
    ok = meck:expect(Mods, test2, 0, ok),
    [?assertEqual(ok, M:test()) || M <- Mods],
    ?assert(meck:validate(Mods)),
    ok = meck:unload(Mods).

multi_invalid_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods, [non_strict]),
    ok = meck:expect(Mods, test, fun(1) -> ok end),
    ?assertError(function_clause, mod2:test(2)),
    ?assert(not meck:validate(Mods)),
    ok = meck:unload(Mods).

multi_option_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods, [passthrough, non_strict]),
    ok = meck:expect(Mods, test, fun() -> ok end),
    [?assertEqual(ok, M:test()) || M <- Mods],
    ?assert(meck:validate(Mods)),
    ok = meck:unload(Mods).

multi_shortcut_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods, [non_strict]),
    ok = meck:expect(Mods, test, 0, ok),
    [?assertEqual(ok, M:test()) || M <- Mods],
    ?assert(meck:validate(Mods)),
    ok = meck:unload(Mods).

multi_delete_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods, [non_strict]),
    ok = meck:expect(Mods, test, 0, ok),
    ?assertEqual(ok, meck:delete(Mods, test, 0)),
    [?assertError(undef, M:test()) || M <- Mods],
    ?assert(meck:validate(Mods)),
    ok = meck:unload(Mods).

multi_expects_test() ->
    Mods = [mod1, mod2, mod3],
    ok = meck:new(Mods, [non_strict]),
    ok = meck:expect(Mods, test, 0, ok),
    ?assertEqual([{mod1, test, 0}, {mod2, test, 0}, {mod3, test, 0}],
                 lists:sort(meck:expects(Mods))),
    ok = meck:unload(Mods).

multi_reset_test() ->
    % Given
    Mods = [mod1, mod2, mod3],
    meck:new(Mods, [non_strict]),
    meck:expect(Mods, test1, 0, ok),
    meck:expect(Mods, test2, 0, ok),
    mod1:test1(),
    mod1:test2(),
    mod2:test1(),
    mod3:test2(),
    % When
    meck:reset(Mods),
    mod1:test1(),
    mod1:test1(),
    % Then
    ?assertMatch([{_Pid, {mod1, test1, []}, ok},
                  {_Pid, {mod1, test1, []}, ok}], meck:history(mod1)),
    ?assertMatch([], meck:history(mod2)),
    ?assertMatch([], meck:history(mod3)).

handle_cast_unmodified_state_test() ->
    S = dummy_state,
    ?assertEqual({noreply, S}, meck_proc:handle_cast(dummy_msg, S)).

code_change_unmodified_state_test() ->
    S = dummy_state,
    ?assertEqual({ok, S}, meck_proc:code_change(old_version, S, [])).

remote_meck_test_() ->
    {foreach, fun remote_setup/0, fun remote_teardown/1,
     [{with, [T]} || T <- [fun remote_meck_/1,
                           fun remote_meck_cover_/1]]}.

remote_setup() ->
    [] = os:cmd("epmd -daemon"),
    Hostname = case node() of
      'nonode@nohost' -> "localhost";
      _               -> test_server_sup:hoststr()
    end,
    Myself = list_to_atom("meck_eunit_test@" ++ Hostname),
    net_kernel:start([Myself, shortnames]),
    {ok, Node} = slave:start_link(list_to_atom(Hostname), meck_remote_test,
                                  "-pa \"" ++ test_dir() ++ "\""),
    {Mod, Bin, File} = code:get_object_code(meck),
    true = rpc:call(Node, code, add_path, [filename:dirname(File)]),
    {module, Mod} = rpc:call(Node, code, load_binary, [Mod, File, Bin]),
    {module, meck_test_module} =
        rpc:call(Node, code, load_file, [meck_test_module]),
    {Node, meck_test_module}.

remote_teardown({Node, _Mod}) ->
    ok = slave:stop(Node),
    ok = net_kernel:stop().

remote_meck_({Node, Mod}) ->
    ?assertEqual(ok, rpc:call(Node, meck, new, [Mod, [no_link, non_strict]])),
    ?assertEqual(ok, rpc:call(Node, meck, expect, [Mod, test, 0, true])),
    ?assertEqual(true, rpc:call(Node, Mod, test, [])).

remote_meck_cover_({Node, Mod}) ->
    {ok, Mod} = cover:compile(test_file(Mod, ".erl")),
    {ok, _Nodes} = cover:start([Node]),
    ?assertEqual(ok, rpc:call(Node, meck, new, [Mod])).

can_mock_sticky_modules_test() ->
    code:stick_mod(meck_test_module),
    meck:new(meck_test_module, [unstick]),
    ?assertNot(code:is_sticky(meck_test_module)),
    meck:unload(meck_test_module),
    ?assert(code:is_sticky(meck_test_module)),
    code:unstick_mod(meck_test_module).


sticky_directory_test_() ->
    {foreach, fun sticky_setup/0, fun sticky_teardown/1,
     [{with, [T]}
      || T <- [fun can_mock_sticky_module_not_yet_loaded_/1,
               fun cannot_mock_sticky_module_without_unstick_/1]]}.

sticky_setup() ->
    % Find out where the beam file is (purge because it is cover compiled)
    Module = meck_test_module,
    false = code:purge(Module),
    {module, Module} = code:load_file(Module),
    Beam = code:which(Module),

    % Unload module so it's not loaded when running meck
    false = code:purge(Module),
    true = code:delete(Module),

    % Create new sticky dir and copy beam file
    Dir = "sticky_test",
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    Dest = filename:join(Dir, filename:basename(Beam)),
    {ok, _BytesCopied} = file:copy(Beam, Dest),
    true = code:add_patha(Dir),
    ok = code:stick_dir(Dir),
    code:load_file(Module),

    {Module, {Dir, Dest}}.

sticky_teardown({Module, {Dir, Dest}}) ->
    % Clean up
    ok = code:unstick_dir(Dir),
    false = code:purge(Module),
    true = code:del_path(Dir),
    ok = file:delete(Dest),
    ok = file:del_dir(Dir).

can_mock_sticky_module_not_yet_loaded_({Mod, _}) ->
    ?assertEqual(ok, meck:new(Mod, [unstick])),
    ?assertNot(code:is_sticky(Mod)),
    ?assertEqual(ok, meck:unload(Mod)),
    ?assert(code:is_sticky(Mod)).

cannot_mock_sticky_module_without_unstick_({Mod, _}) ->
    error_logger:tty(false),
    ?assertError({module_is_sticky, Mod}, meck:new(Mod, [no_link])),
    error_logger:tty(true).

can_mock_non_sticky_module_test() ->
    ?assertNot(code:is_sticky(meck_test_module)),
    ?assertEqual(ok, meck:new(meck_test_module, [unstick])),
    ?assertNot(code:is_sticky(meck_test_module)),
    ?assertEqual(ok, meck:unload(meck_test_module)),
    ?assertNot(code:is_sticky(meck_test_module)).

cannot_expect_bif_or_autogenerated_test() ->
    ?assertEqual(ok, meck:new(unicode, [unstick, passthrough])),
    ?assertError({cannot_mock_builtin, {unicode, characters_to_binary, 2}},
                 meck:expect(unicode, characters_to_binary, 2, doh)),
    ?assertError({cannot_mock_autogenerated, {unicode, module_info, 0}},
                 meck:expect(unicode, module_info, 0, doh)),
    ?assertEqual(ok, meck:unload(unicode)).

meck_module_attributes_test() ->
    ?assertEqual(ok, meck:new(meck_test_module)),
    ?assertEqual([foobar], proplists:get_value(tag,
                                proplists:get_value(attributes,
                                    meck_test_module:module_info()))),
    ?assertEqual(ok, meck:unload(meck_test_module)).

meck_implicit_new_test()->
    meck:expect(meck_test_module, c, [{[1, 1], foo},
                                      {['_', '_'], bar}]),
    ?assertMatch(foo, meck_test_module:c(1, 1)),
    meck:unload().

wait_for_zero_calls_test() ->
    %% Given
    meck:new(test, [non_strict]),
    %% When/Then
    ?assertMatch(ok, meck:wait(0, test, foo, [1, '_'], 100)),
    %% Clean
    meck:unload().

wait_already_called_test() ->
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 2, ok),
    %% When
    test:foo(1, 2),
    test:foo(1, 2),
    %% Then
    ?assertMatch(ok, meck:wait(2, test, foo, [1, '_'], 100)),
    %% Clean
    meck:unload().

wait_not_called_zero_timeout_test() ->
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 2, ok),
    %% When
    test:foo(1, 2),
    test:foo(1, 2),
    %% Then
    ?assertError(timeout, meck:wait(3, test, foo, [1, '_'], 0)),
    %% Clean
    meck:unload().

wait_not_called_another_proc_test() ->
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 2, ok),
    %% When
    test:foo(1, 2), % Called, but not by the expected proc.
    Pid = erlang:spawn(fun() ->
                              test:foo(2, 2) % Unexpected first argument
                       end),
    %% Then
    ?assertError(timeout, meck:wait(1, test, foo, [1, '_'], Pid, 100)),
    %% Clean
    meck:unload().

wait_called_another_proc_test() ->
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 2, ok),
    %% When
    Pid = erlang:spawn(fun() ->
                              timer:sleep(50),
                              test:foo(1, 2),
                              test:foo(2, 2), % Unexpected first argument
                              test:foo(1, 2)
                       end),
    %% Then
    ?assertMatch(ok, meck:wait(2, test, foo, [1, '_'], Pid, 500)),
    %% Clean
    meck:unload().

wait_timeout_test() ->
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 2, ok),
    %% When
    test:foo(1, 2),
    %% Then
    ?assertError(timeout, meck:wait(2, test, foo, [1, '_'], '_', 10)),
    %% Clean
    meck:unload().

wait_for_the_same_pattern_on_different_processes_test() ->
    error_logger:tty(false),
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 2, ok),
    Pid1 = erlang:spawn(fun() ->
                               ?assertMatch(ok,
                                            meck:wait(2, test, foo,
                                                      [1, 2], 100))
                         end),
    MonitorRef1 = erlang:monitor(process, Pid1),
    Pid2 = erlang:spawn(fun() ->
                               ?assertMatch(ok,
                                            meck:wait(3, test, foo,
                                                      [1, 2], 100))
                        end),
    MonitorRef2 = erlang:monitor(process, Pid2),
    %% When
    timer:sleep(50),
    test:foo(1, 2),
    test:foo(1, 2),
    %% Then
    ?assertTerminated(MonitorRef1, normal, 300),
    ?assertTerminated(MonitorRef2, {timeout, _}, 300),
    %% Clean
    meck:unload(),
    error_logger:tty(true).

wait_for_different_patterns_on_different_processes_test() ->
    error_logger:tty(false),
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 1, ok),
    meck:expect(test, bar, 2, ok),
    Pid1 = erlang:spawn(fun() ->
                               ?assertMatch(ok,
                                            meck:wait(2, test, foo,
                                                      [1], 100))
                        end),
    MonitorRef1 = erlang:monitor(process, Pid1),
    Pid2 = erlang:spawn(fun() ->
                               ?assertMatch(ok,
                                            meck:wait(3, test, bar,
                                                      [1, 2], 100))
                        end),
    MonitorRef2 = erlang:monitor(process, Pid2),
    %% When
    timer:sleep(50),
    test:bar(1, 2),
    test:foo(1),
    test:bar(1, 2),
    test:bar(1, 2),
    %% Then
    ?assertTerminated(MonitorRef1, {timeout, _}, 300),
    ?assertTerminated(MonitorRef2, normal, 300),
    %% Clean
    meck:unload(),
    error_logger:tty(true).

wait_purge_expired_tracker_test() ->
    %% Given
    meck:new(test, [non_strict]),
    meck:expect(test, foo, 2, ok),
    ?assertError(timeout, meck:wait(1, test, foo, [1, '_'], 1)),
    %% When
    timer:sleep(50),
    % Makes expired tracker be purged. There is no way to check that from the
    % code only in the coverage report. But at least we exercise this code path
    % here.
    test:foo(1, 2),
    %% Clean
    meck:unload().


meck_passthrough_test_() ->
    {foreach, fun setup_passthrough/0, fun teardown/1,
     [{with, [T]} || T <- [
                           fun delete_passthrough_/1,
                           fun delete_passthrough_force_/1,
                           fun expects_passthrough_/1
                          ]]}.

setup_passthrough() ->
    % Uncomment to run tests with dbg:
    % dbg:tracer(),
    % dbg:p(all, call),
    % dbg:tpl(meck, []),
    ok = meck:new(meck_test_module, [passthrough, non_strict]),
    meck_test_module.

delete_passthrough_(Mod) ->
    ok = meck:expect(Mod, c, 2, {c, d}),
    ?assertMatch({c, d}, Mod:c(a, b)),
    ?assertEqual(ok, meck:delete(Mod, c, 2)),
    ?assertMatch({a, b}, Mod:c(a, b)),
    ?assert(meck:validate(Mod)).

delete_passthrough_force_(Mod) ->
    ok = meck:expect(Mod, c, 2, ok),
    ?assertEqual(ok, meck:delete(Mod, c, 2, true)),
    ?assertError(undef, Mod:test(a, b)),
    ?assert(meck:validate(Mod)).

expects_passthrough_(Mod) ->
    ok = meck:expect(Mod, test, 2, ok),
    ?assertEqual([{Mod, a, 0}, {Mod, b, 0}, {Mod, c, 2}, {Mod, test, 2}],
                 lists:sort(meck:expects(Mod, false))),
    ?assertEqual([{Mod, test, 2}], meck:expects(Mod, true)).


%%=============================================================================
%% Internal Functions
%%=============================================================================

assert_called(Mod, Function, Args, WasCalled) ->
    ?assertEqual(WasCalled, meck:called(Mod, Function, Args)),
    ?assert(meck:validate(Mod)).

assert_called(Mod, Function, Args, Pid, WasCalled) ->
    ?assertEqual(WasCalled, meck:called(Mod, Function, Args, Pid)),
    ?assert(meck:validate(Mod)).

%% @doc Spawn a new process to concurrently call `Fun'. `Fun' is
%% expected to send a request to the specified process, and this
%% function will wait for this message to arrive. (Therefore the
%% process should be suspended and not consuming its message queue.)
%%
%% The returned request handle can be used later in in {@link
%% wait_concurrent_req/1} to wait for the concurrent process to
%% terminate.
concurrent_req(Name, Fun) when is_atom(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            concurrent_req(Pid, Fun);
        undefined ->
            exit(noproc)
    end;
concurrent_req(Pid, Fun) when is_pid(Pid) ->
    {message_queue_len, Msgs} = process_info(Pid, message_queue_len),
    Req = spawn_monitor(Fun),
    wait_message(Pid, Msgs + 1, 100),
    Req.

%% @doc Wait for a concurrent request started with {@link
%% concurrent_req/2} to terminate. The return value is the exit reason
%% of the process.
wait_concurrent_req(Req = {Pid, Monitor}) ->
    receive
        {'DOWN', Monitor, process, Pid, Reason} ->
            Reason
    after
        1000 ->
            exit(Pid, kill),
            wait_concurrent_req(Req)
    end.

wait_message(Pid, _ExpMsgs, Retries) when Retries < 0 ->
    exit(Pid, kill),
    exit(wait_message_timeout);
wait_message(Pid, ExpMsgs, Retries) ->
    {message_queue_len, Msgs} = process_info(Pid, message_queue_len),
    if Msgs >= ExpMsgs ->
            ok;
       true ->
            timer:sleep(1),
            wait_message(Pid, ExpMsgs, Retries - 1)
    end.
