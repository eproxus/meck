-module(meck_test_module).
-tag(foobar).
-deprecated([a/0]).
-export([a/0, b/0, c/2]).

a() -> a.
b() -> b.

c(A, B) ->
    {A, B}.
