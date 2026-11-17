-module(meck_test_module).
-tag(foobar).
%% a/0 is deprecated on purpose, to test that meck handles the
%% -deprecated attribute. Callers in meck_tests suppress ELP W0016.
-deprecated([a/0]).
-export([a/0, b/0, c/2]).

-spec ?MODULE:a() -> a | b.
a() -> a.
b() -> b.

c(A, B) ->
    {A, B}.
