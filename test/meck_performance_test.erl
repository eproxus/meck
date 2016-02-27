%% @doc
-module(meck_performance_test).

%% Interface exports
-export([run/0]).
-export([run/1]).
-export([run/2]).
-export([new_unload/3]).

%%=============================================================================
%% Interface exports
%%=============================================================================

run() -> run(meck, 100).
run(N) -> run(meck, N).
run(Mod, N) ->
    header(Mod),
    run("new+unload", ?MODULE, new_unload, [Mod, test, [non_strict]], N),
    Mod:new(test, [non_strict]),
    run("expect/3", Mod, expect, [test, normal, fun() -> ok end], N),
    run("expect/3+args", Mod, expect, [test, normal_args,
      fun(_, _) -> ok end], N),
    run("expect/4", Mod, expect, [test, shortcut, 0, ok], N),
    run("expect/4+args", Mod, expect, [test, shortcut_args, 2, ok], N),

    header("calls"),
    Mod:expect(test, shortcut_opaque, 0, self()),
    run("normal", test, normal, [], N),
    run("normal_args", test, normal_args, [a, b], N),
    run("shortcut", test, shortcut, [], N),
    run("shortcut_args", test, shortcut_args, [a, b], N),
    run("shortcut_opaque", test, shortcut_opaque, [], N),
    Mod:unload(test),

    header("history"),
    Mod:new(test, [non_strict]),
    Mod:expect(test, func, 1, ok),
    [test:func(I) || I <- lists:seq(1, 100)],
    run("called", Mod, called, [test, func, 50], N),
    Mod:unload(test),
    ok.

new_unload(Mod, Mock, Opts) ->
    Mod:new(Mock, Opts),
    Mod:unload(Mock).

%%=============================================================================
%% Internal functions
%%=============================================================================

header(Name) ->
    io:format("~n~s\t\tMin\tMax\tMed\tAvg~n", [Name]),
    io:format("------------------------------------------------~n").

run(Name, M, F, A, N) ->
    io:fwrite("~-16s", [Name]),
    io:format("~p\t~p\t~p\t~p~n", test_avg(M, F, A, N)).

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    [Min, Max, Med, Avg].

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).
