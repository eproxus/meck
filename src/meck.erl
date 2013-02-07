%%%============================================================================
%%% Copyright 2011 Adam Lindberg & Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%============================================================================

%%% @author Adam Lindberg <eproxus@gmail.com>
%%% @copyright 2011, Adam Lindberg & Erlang Solutions Ltd
%%% @doc Module mocking library for Erlang.

-module(meck).

%% API
-export_type([args_spec/0,
              ret_spec/0,
              func_clause_spec/0]).

%% Interface exports
-export([new/1]).
-export([new/2]).
-export([expect/3]).
-export([expect/4]).
-export([sequence/4]).
-export([loop/4]).
-export([delete/3]).
-export([exception/2]).
-export([passthrough/1]).
-export([history/1]).
-export([history/2]).
-export([validate/1]).
-export([unload/0]).
-export([unload/1]).
-export([called/3]).
-export([called/4]).
-export([num_calls/3]).
-export([num_calls/4]).
-export([reset/1]).

%% Syntactic sugar
-export([loop/1]).
-export([seq/1]).
-export([val/1]).
-export([raise/2]).
-export([passthrough/0]).
-export([exec/1]).
-export([is/1]).

%%%============================================================================
%%% Types
%%%============================================================================

-type meck_mfa() :: {Mod::atom(), Func::atom(), Args::[any()]}.
%% Module, function and arguments that the mock module got called with.

-type stack_trace() :: [{Mod::atom(), Func::atom(), AriOrArgs::byte()|[any()]} |
                        {Mod::atom(), Func::atom(), AriOrArgs::byte()|[any()],
                         Location::[{atom(), any()}]}].
%% Erlang stack trace.

-type history() :: [{CallerPid::pid(), meck_mfa(), Result::any()} |
                    {CallerPid::pid(), meck_mfa(), Class::throw|error|exit,
                     Reason::any(), stack_trace()}].
%% Represents a list of either successful function calls with a returned
%% result or function calls that resulted in an exception with a type,
%% reason and a stack trace. Each tuple begins with the pid of the process
%% that made the call to the function.

-opaque matcher() :: meck_matcher:matcher().
%% Matcher is an entity that is used to check that a particular value meets
%% some criteria. They are used in defining expectation where Erlang patterns
%% are not enough. E.g. to check that a numeric value is within bounds.
%% Instances of `matcher' can be created by {@link is/1} function from either a
%% predicate function or a hamcrest matcher. (see {@link is/1} for details).
%% An instance of this type may be specified in any or even all positions of an
%% {@link arg_spec()}.

-type args_spec() :: [any() | '_' | matcher()].
%% It is used in {@link expect/3} and {@link expect/4} to define an expectation
%% by an argument pattern. The length of the list defines the arity of the
%% function an expectation is created for. Every list element corresponds to a
%% function argument at the respective position. '_' is a wildcard that matches
%% any value. Instead of exact values or '_' wildcards, you can also specify
%% a {@link matcher()} created by {@link is/1} from a predicate function or a
%% hamcrest matcher.

-opaque ret_spec() :: meck_ret_spec:ret_spec().
%% Opaque data structure that specifies a value or a set of values to be returned
%% by a mock stub function defined by either {@link expect/3} and {@link expect/4}.
%% Values of `ret_spec()' are constructed by {@link seq/1}, {@link loop/1},
%% {@link val/1}, and {@link raise/2} functions. They are used to specify
%% return values in {@link expect/3} and {@link expect/4} functions, and also
%% as a parameter of the `stub_all' option of {@link new/2} function.
%%
%% Note that any Erlang term `X' is a valid `ret_spec()' equivalent to
%% `meck:val(X)'.

-type func_clause_spec() :: {args_spec(), ret_spec()}.
%% It is used in {@link expect/3} and {@link expect/4} to define a function
%% clause of complex multi-clause expectations.

%%%============================================================================
%%% Interface exports
%%%============================================================================

%% @equiv new(Mod, [])
-spec new(Mods) -> ok when
      Mods :: Mod | [Mod],
      Mod :: atom().
new(Mod) when is_atom(Mod) -> new(Mod, []);
new(Mod) when is_list(Mod) -> lists:foreach(fun new/1, Mod), ok.

%% @doc Creates new mocked module(s).
%%
%% This replaces the current version (if any) of the modules in `Mod'
%% with an empty module.
%%
%% Since this library is intended to use from test code, this
%% function links a process for each mock to the calling process.
%%
%% The valid options are:
%% <dl>
%%   <dt>`passthrough'</dt>
%%   <dd>Retains the original functions, if not mocked by meck. If used along
%%       with `stub_all' then `stub_all' is ignored.</dd>
%%
%%   <dt>`no_link'</dt>
%%   <dd>Does not link the meck process to the caller process (needed for using
%%       meck in rpc calls).</dd>
%%
%%   <dt>`unstick'</dt>
%%   <dd>Unstick the module to be mocked (e.g. needed for using meck with
%%       kernel and stdlib modules).</dd>
%%
%%   <dt>`no_passthrough_cover'</dt>
%%   <dd>If cover is enabled on the module to be mocked then meck will continue
%%       to capture coverage on passthrough calls. This option allows you to
%%       disable that feature if it causes problems.</dd>
%%
%%   <dt>`{spawn_opt, list()}'</dt>
%%   <dd>Specify Erlang process spawn options. Typically used to specify
%%       non-default, garbage collection options.</dd>
%%
%%   <dt>`no_history'</dt>
%%   <dd>Do not store history of meck calls.</dd>
%%
%%   <dt>`non_strict'</dt>
%%   <dd>A mock created with this option will allow setting expectations on
%%       functions that are not exported from the mocked module. With this
%%       option on it is even possible to mock non existing modules.</dd>
%%
%%   <dt>`{stub_all, '{@link ret_spec()}`}'</dt>
%%   <dd>Stubs all functions exported from the mocked module. The stubs will
%%       return whatever defined by {@link ret_spec()} regardless of arguments
%%       passed in. It is possible to specify this option as just `stub_all'
%%       then stubs will return atom `ok'. If used along with `passthrough'
%%       then `stub_all' is ignored. </dd>
%% </dl>
-spec new(Mods, Options) -> ok when
      Mods :: Mod | [Mod],
      Mod :: atom(),
      Options :: [proplists:property()].
new(Mod, Options) when is_atom(Mod), is_list(Options) ->
    case meck_proc:start(Mod, Options) of
        {ok, _Pid} -> ok;
        {error, Reason} -> erlang:error(Reason, [Mod, Options])
    end;
new(Mod, Options) when is_list(Mod) ->
    lists:foreach(fun(M) -> new(M, Options) end, Mod),
    ok.

%% @doc Add expectation for a function `Func' to the mocked modules `Mod'.
%%
%% An expectation is either of the following:
%% <dl>
%% <dt>`function()'</dt><dd>a stub function that is executed whenever the
%% function `Func' is called. The arity of `function()' identifies for which
%% particular `Func' variant an expectation is created for (that is, a function
%% with arity 2 will generate an expectation for `Func/2').</dd>
%% <dt>`['{@link func_clause_spec()}`]'</dt><dd>a list of {@link
%% arg_spec()}/{@link ret_spec()} pairs. Whenever the function `Func' is called
%% the arguments are matched against the {@link arg_spec()} in the list. As
%% soon as the first match is found then a value defined by the corresponding
%% {@link ret_spec()} is returned.</dd>
%% </dl>
%%
%% It affects the validation status of the mocked module(s). If an
%% expectation is called with the wrong number of arguments or invalid
%% arguments the mock module(s) is invalidated. It is also invalidated if
%% an unexpected exception occurs.
-spec expect(Mods, Func, Expectation) -> ok when
      Mods :: Mod | [Mod],
      Mod :: atom(),
      Func :: atom(),
      Expectation :: function() | [func_clause_spec()].
expect(Mod, Func, Expectation) when is_list(Mod) ->
    lists:foreach(fun(M) -> expect(M, Func, Expectation) end, Mod),
    ok;
expect(_Mod, _Func, []) ->
    erlang:error(empty_clause_list);
expect(Mod, Func, Expectation) when is_atom(Mod), is_atom(Func) ->
    Expect = meck_expect:new(Func, Expectation),
    check_expect_result(meck_proc:set_expect(Mod, Expect)).

%% @doc Adds an expectation with the supplied arity and return value.
%%
%% This creates an expectation which takes `Arity' number of functions
%% and always returns `Result'.
%%
%% @see expect/3.
-spec expect(Mods, Func, AriOrArgs, RetSpec) -> ok when
      Mods :: Mod | [Mod],
      Mod :: atom(),
      Func :: atom(),
      AriOrArgs :: byte() | args_spec(),
      RetSpec :: ret_spec().
expect(Mod, Func, AriOrArgs, RetSpec) when is_list(Mod) ->
    lists:foreach(fun(M) -> expect(M, Func, AriOrArgs, RetSpec) end, Mod),
    ok;
expect(Mod, Func, AriOrArgs, RetSpec) when is_atom(Mod), is_atom(Func) ->
    Expect = meck_expect:new(Func, AriOrArgs, RetSpec),
    check_expect_result(meck_proc:set_expect(Mod, Expect)).

%% @equiv expect(Mod, Func, Ari, seq(Sequence))
%% @deprecated Please use {@link expect/3} or {@link expect/4} along with
%% {@link ret_spec()} generated by {@link seq/1}.
-spec sequence(Mods, Func, Ari, Sequence) -> ok when
      Mods :: Mod | [Mod],
      Mod :: atom(),
      Func :: atom(),
      Ari :: byte(),
      Sequence :: [any()].
sequence(Mod, Func, Ari, Sequence)
  when is_atom(Mod), is_atom(Func), is_integer(Ari), Ari >= 0 ->
    Expect = meck_expect:new(Func, Ari, meck_ret_spec:seq(Sequence)),
    check_expect_result(meck_proc:set_expect(Mod, Expect));
sequence(Mod, Func, Ari, Sequence) when is_list(Mod) ->
    lists:foreach(fun(M) -> sequence(M, Func, Ari, Sequence) end, Mod),
    ok.

%% @equiv expect(Mod, Func, Ari, loop(Loop))
%% @deprecated Please use {@link expect/3} or {@link expect/4} along with
%% {@link ret_spec()} generated by {@link loop/1}.
-spec loop(Mods, Func, Ari, Loop) -> ok when
      Mods :: Mod | [Mod],
      Mod :: atom(),
      Func :: atom(),
      Ari :: byte(),
      Loop :: [any()].
loop(Mod, Func, Ari, Loop)
  when is_atom(Mod), is_atom(Func), is_integer(Ari), Ari >= 0 ->
    Expect = meck_expect:new(Func, Ari, meck_ret_spec:loop(Loop)),
    check_expect_result(meck_proc:set_expect(Mod, Expect));
loop(Mod, Func, Ari, Loop) when is_list(Mod) ->
    lists:foreach(fun(M) -> loop(M, Func, Ari, Loop) end, Mod),
    ok.

%% @doc Deletes an expectation.
%%
%% Deletes the expectation for the function `Func' with the matching
%% arity `Arity'.
-spec delete(Mods, Func, Ari) -> ok when
      Mods :: Mod | [Mod],
      Mod :: atom(),
      Func :: atom(),
      Ari :: byte().
delete(Mod, Func, Ari)
  when is_atom(Mod), is_atom(Func), Ari >= 0 ->
    meck_proc:delete_expect(Mod, Func, Ari);
delete(Mod, Func, Ari) when is_list(Mod) ->
    lists:foreach(fun(M) -> delete(M, Func, Ari) end, Mod),
    ok.

%% @doc Throws an expected exception inside an expect fun.
%%
%% This exception will get thrown without invalidating the mocked
%% module. That is, the code using the mocked module is expected to
%% handle this exception.
%%
%% <em>Note: this code should only be used inside an expect fun.</em>
-spec exception(Class, Reason) -> no_return() when
      Class :: throw | error | exit,
      Reason :: any().
exception(Class, Reason) when Class == throw; Class == error; Class == exit ->
    erlang:throw(meck_ret_spec:raise(Class, Reason)).

%% @doc Calls the original function (if existing) inside an expectation fun.
%%
%% <em>Note: this code should only be used inside an expect fun.</em>
-spec passthrough(Args) -> Result when
      Args :: [any()],
      Result :: any().
passthrough(Args) when is_list(Args) ->
    {Mod, Func} = meck_code_gen:get_current_call(),
    erlang:apply(meck_util:original_name(Mod), Func, Args).

%% @doc Validate the state of the mock module(s).
%%
%% The function returns `true' if the mocked module(s) has been used
%% according to its expectations. It returns `false' if a call has
%% failed in some way. Reasons for failure are wrong number of
%% arguments or non-existing function (undef), wrong arguments
%% (function clause) or unexpected exceptions.
%%
%% Use the {@link history/1} or {@link history/2} function to analyze errors.
-spec validate(Mods) -> boolean() when
      Mods :: Mod | [Mod],
      Mod :: atom().
validate(Mod) when is_atom(Mod) ->
    meck_proc:validate(Mod);
validate(Mod) when is_list(Mod) ->
    not lists:member(false, [validate(M) || M <- Mod]).

%% @doc Return the call history of the mocked module for all processes.
%%
%% @equiv history(Mod, '_')
-spec history(Mod) -> history() when
      Mod :: atom().
history(Mod) when is_atom(Mod) -> meck_history:get_history('_', Mod).

%% @doc Return the call history of the mocked module for the specified process.
%%
%% Returns a list of calls to the mocked module and their results for
%% the specified `Pid'.  Results can be either normal Erlang terms or
%% exceptions that occurred.
%%
%% @see history/1
%% @see called/3
%% @see called/4
%% @see num_calls/3
%% @see num_calls/4
-spec history(Mod, OptCallerPid) -> history() when
      Mod :: atom(),
      OptCallerPid :: '_' | pid().
history(Mod, OptCallerPid)
  when is_atom(Mod), is_pid(OptCallerPid) orelse OptCallerPid == '_' ->
    meck_history:get_history(OptCallerPid, Mod).

%% @doc Unloads all mocked modules from memory.
%%
%% The function returns the list of mocked modules that were unloaded
%% in the process.
-spec unload() -> Unloaded when
      Unloaded :: [Mod],
      Mod :: atom().
unload() -> lists:foldl(fun unload_if_mocked/2, [], registered()).

%% @doc Unload a mocked module or a list of mocked modules.
%%
%% This will purge and delete the module(s) from the Erlang virtual
%% machine. If the mocked module(s) replaced an existing module, this
%% module will still be in the Erlang load path and can be loaded
%% manually or when called.
-spec unload(Mods) -> ok when
      Mods :: Mod | [Mod],
      Mod :: atom().
unload(Mod) when is_atom(Mod) ->
    meck_proc:stop(Mod),
    wait_for_exit(Mod);
unload(Mods) when is_list(Mods) ->
    lists:foreach(fun unload/1, Mods), ok.

%% @doc Returns whether `Mod:Func' has been called with `Args'.
%%
%% @equiv called(Mod, Fun, Args, '_')
-spec called(Mod, OptFun, OptArgs) -> boolean() when
      Mod :: atom(),
      OptFun :: '_' | atom(),
      OptArgs :: '_' | args_spec().
called(Mod, OptFun, OptArgs) ->
    meck_history:num_calls('_', Mod, OptFun, OptArgs) > 0.

%% @doc Returns whether `Pid' has called `Mod:Func' with `Args'.
%%
%% This will check the history for the module, `Mod', to determine
%% whether process `Pid' call the function, `Fun', with arguments, `Args'. If
%% so, this function returns true, otherwise false.
%%
%% Wildcards can be used, at any level in any term, by using the underscore
%% atom: ``'_' ''
%%
%% @see called/3
-spec called(Mod, OptFun, OptArgs, OptCallerPid) -> boolean() when
      Mod :: atom(),
      OptFun :: '_' | atom(),
      OptArgs :: '_' | args_spec(),
      OptCallerPid :: '_' | pid().
called(Mod, OptFun, OptArgs, OptPid) ->
    meck_history:num_calls(OptPid, Mod, OptFun, OptArgs) > 0.

%% @doc Returns the number of times `Mod:Func' has been called with `Args'.
%%
%% @equiv num_calls(Mod, Fun, Args, '_')
-spec num_calls(Mod, OptFun, OptArgs) -> non_neg_integer() when
      Mod :: atom(),
      OptFun :: '_' | atom(),
      OptArgs :: '_' | args_spec().
num_calls(Mod, OptFun, OptArgs) ->
    meck_history:num_calls('_', Mod, OptFun, OptArgs).

%% @doc Returns the number of times process `Pid' has called `Mod:Func'
%%      with `Args'.
%%
%% This will check the history for the module, `Mod', to determine how
%% many times process `Pid' has called the function, `Fun', with
%% arguments, `Args' and returns the result.
%%
%% @see num_calls/3
-spec num_calls(Mod, OptFun, OptArgs, OptCallerPid) -> non_neg_integer() when
      Mod :: atom(),
      OptFun :: '_' | atom(),
      OptArgs :: '_' | args_spec(),
      OptCallerPid :: '_' | pid().
num_calls(Mod, OptFun, OptArgs, OptPid) ->
    meck_history:num_calls(OptPid, Mod, OptFun, OptArgs).

%% @doc Erases the call history for a mocked module or a list of mocked modules.
%%
%% This function will erase all calls made heretofore from the history of the
%% specified modules. It is intended to prevent cluttering of test results with
%% calls to mocked modules made during the test setup phase.
-spec reset(Mods) -> ok when
      Mods :: Mod | [Mod],
      Mod :: atom().
reset(Mod) when is_atom(Mod) ->
    meck_proc:reset(Mod);
reset(Mods) when is_list(Mods) ->
    lists:foreach(fun(Mod) -> reset(Mod) end, Mods).

%% @doc Converts a list of terms into {@link ret_spec()} defining a loop of
%% values. It is intended to be in construction of clause specs for the
%% {@link expect/3} function.
%%
%% Calls to an expect, created with {@link ret_spec()} returned by this function,
%% will return one element at a time from the `Loop' list and will restart at
%% the first element when the end is reached.
-spec loop(Loop) -> ret_spec() when
      Loop :: [ret_spec()].
loop(Loop) -> meck_ret_spec:loop(Loop).

%% @doc Converts a list of terms into {@link ret_spec()} defining a sequence of
%% values. It is intended to be in construction of clause specs for the
%% {@link expect/3} function.
%%
%% Calls to an expect, created with {@link ret_spec} returned by this function,
%% will exhaust the `Sequence' list of return values in order until the last
%% value is reached. That value is then returned for all subsequent calls.
-spec seq(Sequence) -> ret_spec() when
      Sequence :: [ret_spec()].
seq(Sequence) -> meck_ret_spec:seq(Sequence).

%% @doc Converts a term into {@link ret_spec()} defining an individual value.
%% It is intended to be in construction of clause specs for the
%% {@link expect/3} function.
-spec val(Value) -> ret_spec() when
      Value :: any().
val(Value) -> meck_ret_spec:val(Value).

%% @doc Creates a {@link ret_spec()} that defines an exception.
%%
%% Calls to an expect, created with {@link ret_spec()} returned by this function,
%% will raise the specified exception.
-spec raise(Class, Reason) -> ret_spec() when
      Class :: throw | error | exit,
      Reason :: term.
raise(Class, Reason) -> meck_ret_spec:raise(Class, Reason).

%% @doc Creates a {@link ret_spec()} that makes the original module function be
%% called.
%%
%% Calls to an expect, created with {@link ret_spec()} returned by this function,
%% will be forwarded to the original function.
-spec passthrough() -> ret_spec().
passthrough() -> meck_ret_spec:passthrough().

%% @doc Creates a {@link ret_spec()} from a function. Calls to an expect,
%% created with {@link ret_spec()} returned by this function, will be forwarded
%% to the specified function.
-spec exec(fun()) -> ret_spec().
exec(Fun) -> meck_ret_spec:exec(Fun).

%% @doc creates a {@link matcher/0} instance from either `Predicate' or
%% `HamcrestMatcher'.
%% <ul>
%% <li>`Predicate' - is a single parameter function. If it returns `true' then
%% the argument passed to it is considered as meeting the matcher criteria,
%% otherwise as not.</li>
%% <li>`HamcrestMatcher' - is a matcher created by
%% <a href="https://github.com/hyperthunk/hamcrest-erlang">Hamcrest-Erlang</a>
%% library</li>
%% </ul>
-spec is(MatcherImpl) -> matcher() when
      MatcherImpl :: Predicate | HamcrestMatcher,
      Predicate :: fun((any()) -> any()),
      HamcrestMatcher :: hamcrest:matchspec().
is(MatcherImpl) ->
    meck_matcher:new(MatcherImpl).

%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec wait_for_exit(Mod::atom()) -> ok.
wait_for_exit(Mod) ->
    MonitorRef = erlang:monitor(process, meck_util:proc_name(Mod)),
    receive {'DOWN', MonitorRef, _Type, _Object, _Info} -> ok end.

-spec unload_if_mocked(Mod::atom() | string(), Unloaded::[atom()]) ->
        NewUnloaded::[atom()].
unload_if_mocked(Mod, Unloaded) when is_atom(Mod) ->
    unload_if_mocked(atom_to_list(Mod), Unloaded);
unload_if_mocked(ModName, Unloaded) when length(ModName) > 5 ->
    case lists:split(length(ModName) - 5, ModName) of
        {Name, "_meck"} ->
            Mocked = erlang:list_to_existing_atom(Name),
            try
                unload(Mocked)
            catch error:{not_mocked, Mocked} ->
                    ok
            end,
            [Mocked | Unloaded];
        _Else ->
            Unloaded
    end;
unload_if_mocked(_P, Unloaded) ->
    Unloaded.

-spec check_expect_result(ok | {error, Reason::any()}) -> ok.
check_expect_result(ok) -> ok;
check_expect_result({error, Reason}) -> erlang:error(Reason).
