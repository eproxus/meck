%% @doc
-module(meck_performance_test).

%% Interface exports
-export([run/1]).

%%==============================================================================
%% Interface exports
%%==============================================================================

run(N) ->
    meck:new(test),
    meck:expect(test, normal, fun() -> ok end),
    meck:expect(test, normal_args, fun(_, _) -> ok end),
    meck:expect(test, simple, 0, ok),
    meck:expect(test, simple_args, 2, ok),
    io:format("\t\tMin\tMax\tMed\tAvg~n"),
    io:format("normal\t\t~p\t~p\t~p\t~p~n",
              test_avg(test, normal, [], N)),
    io:format("normal_args\t~p\t~p\t~p\t~p~n",
              test_avg(test, normal_args, [a, b], N)),
    io:format("simple\t\t~p\t~p\t~p\t~p~n",
              test_avg(test, simple, [], N)),
    io:format("simple_args\t~p\t~p\t~p\t~p~n",
              test_avg(test, simple_args, [a, b], N)),
    meck:unload(test),
    ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

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
