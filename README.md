meck
====
A mocking library for Erlang.

Introduction
------------
With meck you can easily mock modules in Erlang. Since meck is intended to be used in testing, you can also perform some basic validations on the mocked modules, such as making sure no function is called in a way it should not.

meck automatically renames existing modules in case they are loaded when you want to mock them, and restores them upon unloading of the mocked module. It is also possible to call the original functions from a mocked module using `meck:passthrough/1` from inside an expectation.

Examples
--------
Here's an example of using meck in the Erlang shell:

    Eshell V5.7.5  (abort with ^G)
    1> meck:new(dog).
    ok
    2> meck:expect(dog, bark, fun() -> "Woof!" end).
    ok
    3> dog:bark().
    "Woof!"
    4> meck:validate(dog).
    true
    5> meck:expect(dog, meow, fun() -> meck:exception(error, not_a_cat) end).
    ok
    6> catch dog:meow().
    {'EXIT',{not_a_cat,[{meck,exception,2},
                        {meck,exec,4},
                        {dog,meow,[]},
                        {erl_eval,do_apply,5},
                        {erl_eval,expr,5},
                        {shell,exprs,6},
                        {shell,eval_exprs,6},
                        {shell,eval_loop,3}]}}
    7> meck:validate(dog).
    true
    8> meck:expect(dog, jump, fun(Height) when Height > 3 ->
                                      erlang:error(too_high);
                                 (Height) ->
                                      ok
                              end).
    ok
    9> dog:jump(2).
    ok
    10> catch dog:jump(5).
    {'EXIT',{too_high,[{meck,exec,4},
                       {dog,jump,[5]},
                       {erl_eval,do_apply,5},
                       {erl_eval,expr,5},
                       {shell,exprs,6},
                       {shell,eval_exprs,6},
                       {shell,eval_loop,3}]}}
    11> meck:validate(dog).
    false

Here's an example of using meck inside an EUnit test case:

    my_test() ->
        meck:new(library_module),
        meck:expect(library_module, fib, fun(8) -> 21 end),
        ?assertEqual(21, code_under_test:run(fib, 8)),
        ?assert(meck:validate(library_module)).

Pass-through in action:

    Eshell V5.7.5  (abort with ^G)
    1> code:unstick_mod(string).
    true
    2> meck:new(string).
    ok
    3> meck:expect(string, strip, fun(String) -> meck:passthrough([String]) end).
    ok
    4> string:strip("  test  ").
    "test"
    5> meck:unload(string).
    ok
    6> string:strip("  test  ").
    "test"

Contribute
----------
Should you find yourself using meck and have issues, comments or feedback please [create an issue!] [1]

[1]: http://github.com/eproxus/meck/issues "meck issues"
