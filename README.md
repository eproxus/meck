<h1 align="center">Meck</h1>
<p align="center">A mocking library for Erlang</p>

<p align="center">
  <a href="https://github.com/eproxus/meck/actions/workflows/erlang.yml">
    <img alt="GitHub Actions" src="https://img.shields.io/github/actions/workflow/status/eproxus/meck/erlang.yml?branch=master&style=flat-square"/>
  </a>
  <a href="https://hex.pm/packages/meck">
    <img alt="Hex.pm version" src="https://img.shields.io/hexpm/v/meck?style=flat-square"/>
  </a>
  <a href="LICENSE">
    <img alt="Hex.pm license" src="https://img.shields.io/hexpm/l/meck?style=flat-square"/>
  </a>
  <a href="https://github.com/eproxus/meck/blob/master/.github/workflows/erlang.yml#L14">
    <img alt="Erlang versions" src="https://img.shields.io/badge/erlang-24+-blue.svg?style=flat-square"/>
  </a>
  <a href="https://github.com/sponsors/eproxus">
    <img alt="hex.pm license" src="https://img.shields.io/github/sponsors/eproxus?style=flat-square&color=%23ec6cb9"/>
  </a>
</p>


  * [Features](#features)
  * [Examples](#examples)
  * [Use](#use)
  * [Manual Build](#manual-build)
  * [Caveats](#caveats)
  * [Contribute](#contribute)

## Features

See what's new in [0.8 Release Notes][release_notes_0.8].

  * Dynamic return values using sequences and loops of static values
  * Compact definition of mock arguments, clauses and return values
  * Pass through: call functions in the original module
  * Complete call history showing calls, return values and exceptions
  * Mock validation, will invalidate mocks that were not called correctly
  * Throwing of expected exceptions that keeps the module valid
  * Throws an error when mocking a module that doesn't exist or has been
    renamed (disable with option `non_strict`)
  * Support for [Hamcrest][hamcrest] matchers
  * Automatic backup and restore of cover data
  * Mock is linked to the creating process and will unload automatically
    when a crash occurs (disable with option `no_link`)
  * Mocking of sticky modules (using the option `unstick`)

## Examples

Here's an example of using Meck in the Erlang shell:

```erlang
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

Exceptions can be anticipated by Meck (resulting in validation still passing).
This is intended to be used to test code that can and should handle certain
exceptions indeed does take care of them:

```erlang
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

Normal Erlang exceptions result in a failed validation. The following example is
just to demonstrate the behavior, in real test code the exception would normally
come from the code under test (which should, if not expected, invalidate the
mocked module):

```erlang
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

Pass-through is used when the original functionality of a module should be kept.
When the option `passthrough` is used when calling `new/2` all functions in the
original module will be kept in the mock. These can later be overridden by
calling `expect/3` or `expect/4`.

```erlang
Eshell V5.8.4  (abort with ^G)
1> meck:new(string, [unstick, passthrough]).
ok
2> string:strip("  test  ").
"test"
```

It's also possible to pass calls to the original function allowing us to
override only a certain behavior of a function (this usage is compatible with
the `passthrough` option). `passthrough/1` will always call the original
function with the same name as the expect is defined in):

```erlang
Eshell V5.8.4  (abort with ^G)
1> meck:new(string, [unstick, passthrough]).
ok
2> meck:expect(string, strip, fun
    ("foo") -> "bar";
    (String) -> meck:passthrough([String])
end).
ok
3> string:strip("  test  ").
"test"
4> string:strip("foo").
"bar"
5> meck:unload(string).
ok
5> string:strip("foo").
"foo"
```

## Use

Meck is best used via [Rebar 3][rebar_3]. Add Meck to the test dependencies
in your `rebar.config`:

```erlang
{profiles, [{test, [{deps, [meck]}]}]}.
```

### Manual Build

Meck uses [Rebar 3][rebar_3]. To build Meck go to the Meck directory
and simply type:

```sh
rebar3 compile
```

In order to run all tests for Meck type the following command from the same
directory:

```sh
rebar3 eunit
```

Documentation can be generated through the use of the following command:

```sh
rebar3 edoc
```

### Test Output

Normally the test output is hidden, but if EUnit is run
directly, two things might seem alarming when running the tests:

  1. Warnings emitted by cover
  2. An exception printed by SASL

Both are expected due to the way Erlang currently prints errors. The important
line you should look for is `All XX tests passed`, if that appears all is
correct.

## Caveats

### Global Namespace

Meck will have trouble mocking certain modules since it works by recompiling
and reloading modules in the global Erlang module namespace. Replacing a
module affects the whole Erlang VM and any running processes using that
module. This means certain modules cannot be mocked or will cause trouble.

In general, if a module is used by running processes or include Native
Implemented Functions (NIFs) they will be hard or impossible to mock. You may
be lucky and it could work, until it breaks one day.

The following is a non-exhaustive
list of modules that can either be problematic to mock or not possible at
all:

* `erlang`
* `supervisor`
* All `gen_` family of modules (`gen_server`, `gen_statem` etc.)
* `os`
* `crypto`
* `compile`
* `global`
* `timer` (possible to mock, but used by some test frameworks, like Elixir's
  ExUnit)

### Local Functions

A meck expectation set up for a function _f_ does not apply to the module-
local invocation of _f_ within the mocked module. Consider the following module:

```erlang
-module(test).
-export([a/0, b/0, c/0]).

a() ->
  c().

b() ->
  ?MODULE:c().

c() ->
  original.
```

Note how the module-local call to `c/0` in `a/0` stays unchanged even though the
expectation changes the externally visible behaviour of `c/0`:

```erlang
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

### Common Test

When using `meck` under Erlang/OTP's Common Test, one should pay special
attention to this bit in the chapter on
[Writing Tests](https://erlang.org/doc/apps/common_test/write_test_chapter.html):

> `init_per_suite` and `end_per_suite` execute on dedicated Erlang processes,
> just like the test cases do.

Common Test runs `init_per_suite` in an isolated process which terminates when
done, before the test case runs. A mock that is created there will also
terminate and unload itself before the test case runs. This is because it is
linked to the process creating it. This can be especially tricky to detect if
`passthrough` is used when creating the mock, since it is hard to know if it is
the mock responding to function calls or the original module.

To avoid this, you can pass the `no_link` flag to `meck:new/2` which will unlink
the mock from the process that created it. When using `no_link` you should make
sure that `meck:unload/1` is called properly (for all test outcomes, or
crashes) so that a left-over mock does not interfere with subsequent test
cases.

## Contribute

Patches are greatly appreciated! For a much nicer history, please [write good
commit messages][commit_messages]. Use a branch name prefixed by `feature/`
(e.g. `feature/my_example_branch`) for easier integration when developing new
features or fixes for meck.

Should you find yourself using Meck and have issues, comments or feedback please
[create an issue here on GitHub][issues].

Meck has been greatly improved by [many contributors](https://github.com/eproxus/meck/graphs/contributors)!

### Donations

If you or your company use Meck and find it useful, a [sponsorship][sponsors] or [donations][liberapay] are greatly appreciated!

<noscript>
  <span>
   <a href="https://github.com/sponsors/eproxus">
   <img alt="Sponsor on GitHub"
        src="https://img.shields.io/github/sponsors/eproxus?label=Sponsor&color=EA4AAA&logo=GitHub%20Sponsors&style=social">
   </a>
  </span>
  <span>
    <a href="https://liberapay.com/eproxus/donate">
      <img alt="Donate using Liberapay"
           src="https://liberapay.com/assets/widgets/donate.svg">
    </a>
  </span>
</noscript>

<!-- Links -->
[release_notes_0.8]: https://github.com/eproxus/meck/wiki/0.8-Release-Notes
[hamcrest]: https://github.com/hyperthunk/hamcrest-erlang
[rebar_3]: https://github.com/erlang/rebar3
[issues]: http://github.com/eproxus/meck/issues
[commit_messages]: http://chris.beams.io/posts/git-commit/
[sponsors]: https://github.com/sponsors/eproxus
[liberapay]: https://liberapay.com/eproxus/
