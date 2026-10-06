<!-- markdownlint-disable-line MD013 -->
# Meck [![CI Status][ci-img]](https://github.com/eproxus/meck/actions/workflows/erlang.yml?query=branch%3Amaster) [![Hex.pm Version][hex-img]](https://hex.pm/packages/meck) [![Docs][docs-img]](https://hexdocs.pm/meck) [![Minimum Erlang Version][erlang-img]](https://github.com/eproxus/meck/blob/master/.github/workflows/erlang.yml#L14) [![License][license-img]](LICENSE) [![GitHub Sponsors][sponsors-img]](https://github.com/sponsors/eproxus)

A mocking library for Erlang.

## Features

* Flexible, dynamic expectations
    * Adding, listing, merging (with the `merge_expects` option) and deleting at
      runtime
    * Compact definition of function arguments or dynamic matchers, clauses
      and return values
    * Dynamic return values using sequences and loops of static values
    * Passthrough calls to the original module
    * Exceptions designated as intentional keep the module valid
    * Customizable default return value for all functions of the mocked module
      (with the `stub_all` option)
* Full call history access
    * Complete call history showing calls, return values and exceptions
    * Capture of individual argument values from specific calls
    * Waiting for specific calls to the mock, with a timeout
    * History reset
    * Disabling of history recording for performance-critical tests (with the
      `no_history` option)
* Intuitive mock lifecycle management
    * Protection against mocking modules that do not exist (disable with the
      `non_strict` option)
    * Automatic mock creation for existing modules when mocking functions
    * Invalidation of mocks that are not called correctly
    * Automatic unloading when the creating process crashes (disable with the
      `no_link` option)
    * Listing of all current mocks
    * Mocking of sticky modules (with the `unstick` option)
    * Automatic backup and restore of cover data, with the possibility to
      disable cover on passthrough calls (with the `no_passthrough_cover`
      option)
* Batch operations on several modules at once

## Usage

### Basic Usage

Here's an example of using Meck in the Erlang shell:

```erlang
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

### Exceptions

Exceptions can be anticipated by Meck (resulting in validation still passing).
This is intended to be used to test code that can and should handle certain
exceptions indeed does take care of them:

```erlang
1> meck:new(dog, [non_strict]).
ok
2> meck:expect(dog, meow, fun() -> meck:exception(error, not_a_cat) end).
ok
3> catch dog:meow().
{'EXIT',{not_a_cat,[...]}}
4> meck:validate(dog).
true
```

Normal Erlang exceptions result in a failed validation. The following example is
just to demonstrate the behavior, in real test code the exception would normally
come from the code under test (which should, if not expected, invalidate the
mocked module):

```erlang
1> meck:new(dog, [non_strict]).
ok
2> meck:expect(dog, jump, fun(Height) when Height =< 3 -> ok end).
ok
3> dog:jump(2).
ok
4> catch dog:jump(5).
{'EXIT',{function_clause,[...]}}
5> meck:validate(dog).
false
```

### EUnit

Here's an example of using Meck inside an EUnit test case:

```erlang
my_test() ->
    meck:new(my_library_module),
    meck:expect(my_library_module, fib, fun(8) -> 21 end),
    ?assertEqual(21, code_under_test:run(fib, 8)), % Uses my_library_module
    ?assert(meck:validate(my_library_module)),
    meck:unload(my_library_module).
```

### Passthrough

Pass-through is used when the original functionality of a module should be kept.
When the option `passthrough` is used when calling `new/2` all functions in the
original module will be kept in the mock. These can later be overridden by
calling `expect/3` or `expect/4`.

```erlang
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
6> string:strip("foo").
"foo"
```

## Installation

Meck is best used via [Rebar 3][rebar3]. Add Meck to the test dependencies in
your `rebar.config`:

```erlang
{profiles, [{test, [{deps, [meck]}]}]}.
```

### Build

Meck uses [Rebar 3][rebar3]. To build Meck go to the Meck directory and simply
type:

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

Normally the test output is hidden, but if EUnit is run directly, two things
might seem alarming when running the tests:

1. Warnings emitted by cover
2. An exception printed by SASL

Both are expected due to the way Erlang currently prints errors. The important
line you should look for is `All XX tests passed`, if that appears all is
correct.

## Caveats

### Global Namespace

Meck will have trouble mocking certain modules since it works by recompiling and
reloading modules in the global Erlang module namespace. Replacing a module
affects the whole Erlang VM and any running processes using that module. This
means certain modules cannot be mocked or will cause trouble.

In general, if a module is used by running processes or include Native
Implemented Functions (NIFs) they will be hard or impossible to mock. You may be
lucky and it could work, until it breaks one day.

The following is a non-exhaustive list of modules that can either be problematic
to mock or not possible at all:

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

A Meck expectation set up for a function does not apply to the module- local
invocation of that function within the mocked module. Consider the following
module:

```erlang
-module(test).
-export([a/0, b/0, c/0]).

a() -> c().

b() -> ?MODULE:c(). % This is a fully qualified call

c() -> original.
```

Note how the module-local call to `c/0` in `a/0` stays unchanged even though the
expectation changes the externally visible behaviour of `c/0`:

```erlang
1> c(test, [debug_info]).
{ok,test}
2> meck:new(test, [passthrough]).
ok
3> meck:expect(test, c, 0, changed).
ok
4> test:a().
original
5> test:b().
changed
6> test:c().
changed
```

### Common Test

When using Meck under Erlang/OTP's Common Test, one should pay special attention
to this bit in the chapter on [Writing Tests][ct-writing-tests]:

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
sure that `meck:unload/1` is called properly (for all test outcomes, or crashes)
so that a left-over mock does not interfere with subsequent test cases.

## Contributing

Patches are greatly appreciated! For a much nicer history, please [write good
commit messages][commit-messages]. Use a branch name prefixed by `feature/`
(e.g. `feature/my_example_branch`) for easier integration when developing new
features or fixes for Meck.

Should you find yourself using Meck and have issues, comments or feedback please
[create an issue here on GitHub][issues].

Meck has been greatly improved by [many contributors][contributors]!

For more information check out [CONTRIBUTING.md][contributing].

### Donations

If you or your company use Meck and find it useful, a [sponsorship][sponsors] or
[donations][liberapay] are greatly appreciated!

[![Sponsor on GitHub][sponsor-button-img]](https://github.com/sponsors/eproxus)
[![Donate using Liberapay][liberapay-button-img]](https://liberapay.com/eproxus/donate)

## Changelog

See [CHANGELOG][changelog] or the [Releases][releases] page.

## Code of Conduct

Find this project's code of conduct in
[Contributor Covenant Code of Conduct][code-of-conduct].

## Conventions

### Versions

This project adheres to [Semantic Versioning][semver].

### License

This project uses the [Apache License 2.0][license].

[ci-img]:               https://img.shields.io/github/actions/workflow/status/eproxus/meck/erlang.yml?label=ci
[hex-img]:              https://img.shields.io/hexpm/v/meck
[docs-img]:             https://img.shields.io/badge/docs-hexdocs-blue
[erlang-img]:           https://img.shields.io/badge/erlang-27+-blue.svg
[license]:              LICENSE
[license-img]:          https://img.shields.io/hexpm/l/meck
[sponsors]:             https://github.com/sponsors/eproxus
[sponsors-img]:         https://img.shields.io/github/sponsors/eproxus?color=%23ec6cb9
[rebar3]:               https://github.com/erlang/rebar3
[ct-writing-tests]:     https://erlang.org/doc/apps/common_test/write_test_chapter.html
[commit-messages]:      http://chris.beams.io/posts/git-commit/
[issues]:               http://github.com/eproxus/meck/issues
[contributors]:         https://github.com/eproxus/meck/graphs/contributors
[contributing]:         CONTRIBUTING.md
[liberapay]:            https://liberapay.com/eproxus/
[sponsor-button-img]:   https://img.shields.io/github/sponsors/eproxus?label=Sponsor&color=EA4AAA&logo=GitHub%20Sponsors&style=social
[liberapay-button-img]: https://liberapay.com/assets/widgets/donate.svg
[changelog]:            https://github.com/eproxus/meck/blob/master/CHANGELOG.md
[releases]:             https://github.com/eproxus/meck/releases
[code-of-conduct]:      CODE_OF_CONDUCT.md
[semver]:               https://semver.org/spec/v2.0.0.html
