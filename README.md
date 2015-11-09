[![Release](http://img.shields.io/github/release/eproxus/meck.svg?style=flat-square)](https://github.com/eproxus/meck/releases/latest)
[![Build Status](http://img.shields.io/travis/eproxus/meck.svg?style=flat-square)](http://travis-ci.org/eproxus/meck)
[![Code Climate](http://img.shields.io/badge/code_climate-Erlang_17.4-brightgreen.svg?style=flat-square)](https://travis-ci.org/eproxus/meck)

Meck
====

A mocking library for Erlang.

<a name='features'>

Features
--------

See what's new in [0.8 Release Notes][1].

  * Dynamic return values using sequences and loops of static values
  * Compact definition of mock arguments, clauses and return values
  * Pass through: call functions in the original module
  * Complete call history showing calls, return values and exceptions
  * Mock validation, will invalidate mocks that were not called correctly
  * Throwing of expected exceptions that keeps the module valid
  * Throws an error when mocking a module that doesn't exist or has been
    renamed (disable with option `non_strict`)
  * Support for [Hamcrest][2] matchers
  * Automatic backup and restore of cover data
  * Mock is linked to the creating process and will unload automatically
    when a crash occurs (disable with option `no_link`)
  * Mocking of sticky modules (using the option `unstick`)


<a name='examples'>

Examples
--------
Here's an example of using Meck in the Erlang shell:

```erl
Eshell V5.8.4  (abort with ^G)
1> meck:new(dog, [non_strict]). % non_strict is used to create modules that don't exist
ok
2> meck:expect(dog, bark, fun() -> "Woof!" end).
ok
3> dog:bark().
"Woof!"
4> meck:validate(dog).
true
5> meck:unload(dog).
ok
6> dog:bark().
** exception error: undefined function dog:bark/0
```

Exceptions can be anticipated by Meck (resulting in validation still
passing). This is intended to be used to test code that can and should
handle certain exceptions indeed does take care of them:

```erl
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
```

Normal Erlang exceptions result in a failed validation. The following
example is just to demonstrate the behavior, in real test code the
exception would normally come from the code under test (which should,
if not expected, invalidate the mocked module):

```erl
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
```

Here's an example of using Meck inside an EUnit test case:

```erlang
my_test() ->
    meck:new(my_library_module),
    meck:expect(my_library_module, fib, fun(8) -> 21 end),
    ?assertEqual(21, code_under_test:run(fib, 8)), % Uses my_library_module
    ?assert(meck:validate(my_library_module)),
    meck:unload(my_library_module).
```

Pass-through is used when the original functionality of a module
should be kept. When the option `passthrough` is used when calling
`new/2` all functions in the original module will be kept in the
mock. These can later be overridden by calling `expect/3` or
`expect/4`.

```erl
Eshell V5.8.4  (abort with ^G)
1> meck:new(string, [unstick, passthrough]).
ok
2> string:strip("  test  ").
"test"
```

It's also possible to pass calls to the original function allowing us
to override only a certain behavior of a function (this usage is
compatible with the `passthrough` option). `passthrough/1` will always
call the original function with the same name as the expect is 
defined in):

```erl
Eshell V5.8.4  (abort with ^G)
1> meck:new(string, [unstick]).
ok
2> meck:expect(string, strip, fun(String) -> meck:passthrough([String]) end).
ok
3> string:strip("  test  ").
"test"
4> meck:unload(string).
ok
5> string:strip("  test  ").
"test"
```

<a name='build'>

Build
-----

Meck requires `make` and [rebar][1] to build. To build Meck go to the Meck directory
and simply type:

```sh
make
```

In order to run all tests for Meck type the following command from the same directory:

```sh
make test
```

Two things might seem alarming when running the tests:

  1. Warnings emitted by cover
  2. An exception printed by SASL

Both are expected due to the way Erlang currently prints errors. The
important line you should look for is `All XX tests passed`, if that
appears all is correct.

Documentation can be generated through the use of the following command:

```sh
make doc
```

<a name='install'>

Install
-------

Meck is best used via [rebar][3]. Add the following dependency t
your `rebar.config` in your project root:

```erlang
{deps, [
 {meck, ".*",
  {git, "https://github.com/eproxus/meck.git", {tag, "0.8.3"}}}
 ]}.
```

If you want to install your own built version of Meck add the ebin
directory to your Erlang code path or move the Meck folder into your
release folder and make sure that folder is in your `ERL_LIBS`
environment variable.


<a name='contribute'>

Caveats
-------

Meck will have trouble mocking certain modules since Meck works by
recompiling and reloading modules. Since Erlang have a flat module
namespace, replacing a module has to be done globally in the
Erlang VM. This means certain modules cannot be mocked. The
following is a non-exhaustive list of modules that can either be
problematic to mock or not possible at all:

* `erlang`
* `os`
* `crypto`
* `compile`
* `global`
* `timer` (possible to mock, but used by some test frameworks, like Elixir's ExUnit)

Also, a meck expectation set up for a function _f_ does not apply to the module-local invocation of _f_ within the mocked module.
Consider the following module:
```
-module(test).
-export([a/0, b/0, c/0]).

a() ->
  c().

b() ->
  ?MODULE:c().

c() ->
  original.
```
Note how the module-local call to `c/0` in `a/0` stays unchanged even though the expectation changes the externally visible behaviour of `c/0`:

```
3> meck:new(test, [passthrough]).
ok
4> meck:expect(test,c,0,changed).
ok
5> test:a().
original
6> test:b().
changed
6> test:c().
changed
```

Contribute
----------

Patches are greatly appreciated! For a much nicer history, please
[write good commit messages][5]. Use a branch name prefixed by
`feature/` (e.g. `feature/my_example_branch`) for easier integration
when developing new features or fixes for meck.

Should you find yourself using Meck and have issues, comments or
feedback please [create an issue here on GitHub][4].

Meck has been greatly improved by [many contributors]
(https://github.com/eproxus/meck/graphs/contributors)!

### Donations

If you or your company use Meck and find it useful, Bitcoin donations to the address `1M7pLbBpjkwxffT7kZPKhxiPGKf4eHDqtz` are greatly appreciated!

  [1]: https://github.com/eproxus/meck/wiki/0.8-Release-Notes
       "0.8 Release Notes"
  [2]: https://github.com/hyperthunk/hamcrest-erlang "Hamcrest for Erlang"
  [3]: https://github.com/basho/rebar "Rebar - A build tool for Erlang"
  [4]: http://github.com/eproxus/meck/issues "Meck issues"

