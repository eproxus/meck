-module(meck_test_module).
-export([a/0, b/0, c/2]).

a() -> a.
b() -> b.

c(A, B) ->
    {A, B}.
