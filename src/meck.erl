%%==============================================================================
%% Copyright 2011 Adam Lindberg & Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%% @author Adam Lindberg <eproxus@gmail.com>
%% @copyright 2011, Adam Lindberg & Erlang Solutions Ltd
%% @doc Module mocking library for Erlang.

-module(meck).
-behaviour(gen_server).

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
-export([validate/1]).
-export([unload/0]).
-export([unload/1]).
-export([called/3]).

%% Callback exports
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([exec/4]).

%% Types
%% @type meck_mfa() = {Mod::atom(), Func::atom(), Args::list(term())}.
%% Module, function and arguments that the mock module got called with.
-type meck_mfa() :: {Mod::atom(), Func::atom(), Args::[term()]}.

%% @type history() = [{meck_mfa(), Result::term()}
%%                     | {meck_mfa(), Class:: exit | error | throw,
%%                        Reason::term(), Stacktrace::list(mfa())}].
%% History is a list of either successful function calls with a
%% returned result or function calls that resulted in an exception
%% with a type, reason and a stack trace.
-type history() :: [{meck_mfa(), Result::term()}
                    | {meck_mfa(), Class:: exit | error | throw,
                       Reason::term(), Stacktrace::[mfa()]}].

%% Records
-record(state, {mod :: atom(),
                expects :: dict(),
                valid = true :: boolean(),
                history = [] :: history(),
                original :: term(),
                was_sticky :: boolean()}).

%% Includes
-include("meck_abstract.hrl").

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @spec new(Mod:: atom() | list(atom())) -> ok
%% @equiv new(Mod, [])
-spec new(Mod:: atom() | [atom()]) -> ok.
new(Mod) when is_atom(Mod) -> new(Mod, []);
new(Mod) when is_list(Mod) -> lists:foreach(fun new/1, Mod), ok.

%% @spec new(Mod:: atom() | list(atom()), Options::list(term())) -> ok
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
%%   <dt>`passthrough'</dt><dd>Retains the original functions, if not
%%                             mocked by meck.</dd>
%%   <dt>`no_link'</dt>    <dd>Does not link the meck process to the caller
%%                             process (needed for using meck in rpc calls).
%%                         </dd>
%%   <dt>`unstick'</dt>    <dd>Unstick the module to be mocked (e.g. needed
%%                             for using meck with kernel and stdlib modules).
%%                         </dd>
%% </dl>
-spec new(Mod:: atom() | [atom()], Options::[term()]) -> ok.
new(Mod, Options) when is_atom(Mod), is_list(Options) ->
    case start(Mod, Options) of
        {ok, _Pid} -> ok;
        {error, Reason} -> erlang:error(Reason)
    end;
new(Mod, Options) when is_list(Mod) ->
    lists:foreach(fun(M) -> new(M, Options) end, Mod),
    ok.

%% @spec expect(Mod:: atom() | list(atom()), Func::atom(), Expect::fun()) -> ok
%% @doc Add expectation for a function `Func' to the mocked modules `Mod'.
%%
%% An expectation is a fun that is executed whenever the function
%% `Func' is called.
%%
%% It affects the validation status of the mocked module(s). If an
%% expectation is called with the wrong number of arguments or invalid
%% arguments the mock module(s) is invalidated. It is also invalidated if
%% an unexpected exception occurs.
-spec expect(Mod:: atom() | [atom()], Func::atom(), Expect::fun()) -> ok.
expect(Mod, Func, Expect)
  when is_atom(Mod), is_atom(Func), is_function(Expect) ->
    call(Mod, {expect, Func, Expect});
expect(Mod, Func, Expect) when is_list(Mod) ->
    lists:foreach(fun(M) -> expect(M, Func, Expect) end, Mod),
    ok.

%% @spec expect(Mod:: atom() | list(atom()), Func::atom(),
%%              Arity::pos_integer(), Result::term()) -> ok
%% @doc Adds an expectation with the supplied arity and return value.
%%
%% This creates an expectation which takes `Arity' number of functions
%% and always returns `Result'.
%%
%% @see expect/3.
-spec expect(Mod:: atom() | [atom()], Func::atom(),
             Arity::pos_integer(), Result::term()) -> ok.
expect(Mod, Func, Arity, Result)
  when is_atom(Mod), is_atom(Func), is_integer(Arity), Arity >= 0 ->
    call(Mod, {expect, Func, Arity, Result});
expect(Mod, Func, Arity, Result) when is_list(Mod) ->
    lists:foreach(fun(M) -> expect(M, Func, Arity, Result) end, Mod),
    ok.

%% @spec sequence(Mod:: atom() | list(atom()), Func::atom(),
%%                Arity::pos_integer(), Sequence::[term()]) -> ok
%% @doc Adds an expectation which returns a value from `Sequence'
%% until exhausted.
%%
%% This creates an expectation which takes `Arity' number of arguments
%% and returns one element from `Sequence' at a time. Thus, calls to
%% this expect will exhaust the list of return values in order until
%% the last value is reached. That value is then returned for all
%% subsequent calls.
-spec sequence(Mod:: atom() | [atom()], Func::atom(),
               Arity::pos_integer(), Sequence::[term()]) -> ok.
sequence(Mod, Func, Arity, Sequence)
  when is_atom(Mod), is_atom(Func), is_integer(Arity), Arity >= 0 ->
    call(Mod, {sequence, Func, Arity, Sequence});
sequence(Mod, Func, Arity, Sequence) when is_list(Mod) ->
    lists:foreach(fun(M) -> sequence(M, Func, Arity, Sequence) end, Mod),
    ok.

%% @spec loop(Mod:: atom() | list(atom()), Func::atom(),
%%            Arity::pos_integer(), Loop::[term()]) -> ok
%% @doc Adds an expectation which returns a value from `Loop'
%% infinitely.
%%
%% This creates an expectation which takes `Arity' number of arguments
%% and returns one element from `Loop' at a time. Thus, calls to this
%% expect will return one element at a time from the list and will
%% restart at the first element when the end is reached.
-spec loop(Mod:: atom() | [atom()], Func::atom(),
           Arity::pos_integer(), Loop::[term()]) -> ok.
loop(Mod, Func, Arity, Loop)
  when is_atom(Mod), is_atom(Func), is_integer(Arity), Arity >= 0 ->
    call(Mod, {loop, Func, Arity, Loop});
loop(Mod, Func, Arity, Loop) when is_list(Mod) ->
    lists:foreach(fun(M) -> loop(M, Func, Arity, Loop) end, Mod),
    ok.

%% @spec delete(Mod:: atom() | list(atom()), Func::atom(),
%%              Arity::pos_integer()) -> ok
%% @doc Deletes an expectation.
%%
%% Deletes the expectation for the function `Func' with the matching
%% arity `Arity'.
-spec delete(Mod:: atom() | [atom()], Func::atom(), Arity::pos_integer()) ->
    ok.
delete(Mod, Func, Arity)
  when is_atom(Mod), is_atom(Func), Arity >= 0 ->
    call(Mod, {delete, Func, Arity});
delete(Mod, Func, Arity) when is_list(Mod) ->
    lists:foreach(fun(M) -> delete(M, Func, Arity) end, Mod),
    ok.

%% @spec exception(Class:: throw | error | exit, Reason::term()) -> no_return()
%% @doc Throws an expected exception inside an expect fun.
%%
%% This exception will get thrown without invalidating the mocked
%% module. That is, the code using the mocked module is expected to
%% handle this exception.
%%
%% <em>Note: this code should only be used inside an expect fun.</em>
-spec exception(Class:: throw | error | exit, Reason::term()) -> no_return().
exception(Class, Reason) when Class == throw; Class == error; Class == exit ->
    throw(mock_exception_fun(Class, Reason)).

%% @spec passthrough(Args::list(term())) -> no_return()
%% @doc Calls the original function (if existing) inside an expectation fun.
%%
%% This call does not return, thus everything after this call inside
%% an expectation fun will be ignored.
%%
%% <em>Note: this code should only be used inside an expect fun.</em>
-spec passthrough(Args::[term()]) -> no_return().
passthrough(Args) -> throw(passthrough_fun(Args)).

%% @spec validate(Mod:: atom() | list(atom())) -> boolean()
%% @doc Validate the state of the mock module(s).
%%
%% The function returns `true' if the mocked module(s) has been used
%% according to its expectations. It returns `false' if a call has
%% failed in some way. Reasons for failure are wrong number of
%% arguments or non-existing function (undef), wrong arguments
%% (function clause) or unexpected exceptions.
%%
%% Use the {@link history/1} function to analyze errors.
-spec validate(Mod:: atom() | [atom()]) -> boolean().
validate(Mod) when is_atom(Mod) ->
    call(Mod, validate);
validate(Mod) when is_list(Mod) ->
    not lists:member(false, [validate(M) || M <- Mod]).

%% @spec history(Mod::atom()) -> history()
%% @doc Return the call history of the mocked module.
%%
%% Returns a list of calls to the mocked module and their
%% results. Results can be either normal Erlang terms or exceptions
%% that occurred.
-spec history(Mod::atom()) -> history().
history(Mod) when is_atom(Mod) -> call(Mod, history).

%% @spec unload() -> list(atom())
%% @doc Unloads all mocked modules from memory.
%%
%% The function returns the list of mocked modules that were unloaded
%% in the process.
-spec unload() -> [atom()].
unload() -> lists:foldl(fun unload_if_mocked/2, [], registered()).

%% @spec unload(Mod:: atom() | list(atom())) -> ok
%% @doc Unload a mocked module or a list of mocked modules.
%%
%% This will purge and delete the module(s) from the Erlang virtual
%% machine. If the mocked module(s) replaced an existing module, this
%% module will still be in the Erlang load path and can be loaded
%% manually or when called.
-spec unload(Mods:: atom() | [atom()]) -> ok.
unload(Mod) when is_atom(Mod) -> call(Mod, stop), wait_for_exit(Mod);
unload(Mods) when is_list(Mods) -> lists:foreach(fun unload/1, Mods), ok.

%% @spec called(Mod:: atom(), Fun:: atom(), Args:: list(term())) -> boolean()
%% @doc Returns whether `Mod:Func' has been called with `Args'.
%%
%% This will check the history for the module, `Mod', to determine
%% whether the function, `Fun', was called with arguments, `Args'. If
%% so, this function returns true, otherwise false.
-spec called(Mod::atom(), Fun::atom(), Args::list()) -> boolean().
called(Mod, Fun, Args) ->
    has_call({Mod, Fun, Args}, meck:history(Mod)).

%%==============================================================================
%% Callback functions
%%==============================================================================

%% @hidden
init([Mod, Options]) ->
    WasSticky = case proplists:is_defined(unstick, Options) of
        true -> {module, Mod} = code:ensure_loaded(Mod),
                unstick_original(Mod);
        _    -> false
    end,
    Original = backup_original(Mod),
    process_flag(trap_exit, true),
    Expects = init_expects(Mod, Options),
    try
        meck_mod:compile_and_load_forms(to_forms(Mod, Expects)),
        {ok, #state{mod = Mod, expects = Expects, original = Original,
                    was_sticky = WasSticky}}
    catch
        exit:{error_loading_module, Mod, sticky_directory} ->
            {stop, module_is_sticky}
    end.

%% @hidden
handle_call({get_expect, Func, Arity}, _From, S) ->
    {Expect, NewExpects} = get_expect(S#state.expects, Func, Arity),
    {reply, Expect, S#state{expects = NewExpects}};
handle_call({expect, Func, Expect}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, Expect, S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call({expect, Func, Arity, Result}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, {anon, Arity, Result},
                              S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call({sequence, Func, Arity, Sequence}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, {sequence, Arity, Sequence},
                              S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call({loop, Func, Arity, Loop}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, {loop, Arity, Loop, Loop},
                              S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call({delete, Func, Arity}, _From, S) ->
    NewExpects = delete_expect(S#state.mod, Func, Arity, S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call(history, _From, S) ->
    {reply, lists:reverse(S#state.history), S};
handle_call(invalidate, _From, S) ->
    {reply, ok, S#state{valid = false}};
handle_call(validate, _From, S) ->
    {reply, S#state.valid, S};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S}.

%% @hidden
handle_cast({add_history, Item}, S) ->
    {noreply, S#state{history = [Item| S#state.history]}};
handle_cast(_Msg, S)  ->
    {noreply, S}.

%% @hidden
handle_info(_Info, S) -> {noreply, S}.

%% @hidden
terminate(_Reason, #state{mod = Mod, original = OriginalState, was_sticky = WasSticky}) ->
    cleanup(Mod),
    restore_original(Mod, OriginalState, WasSticky),
    ok.

%% @hidden
code_change(_OldVsn, S, _Extra) -> {ok, S}.

%% @hidden
exec(Mod, Func, Arity, Args) ->
    Expect = call(Mod, {get_expect, Func, Arity}),
    try Result = call_expect(Mod, Func, Expect, Args),
        cast(Mod, {add_history, {{Mod, Func, Args}, Result}}),
        Result
    catch
        throw:Fun when is_function(Fun) ->
            case is_mock_exception(Fun) of
                true  -> handle_mock_exception(Mod, Func, Fun, Args);
                false -> invalidate_and_raise(Mod, Func, Args, throw, Fun)
            end;
        Class:Reason ->
            invalidate_and_raise(Mod, Func, Args, Class, Reason)
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

%% --- Process functions -------------------------------------------------------

start(Mod, Options) ->
    case proplists:is_defined(no_link, Options) of
        true  -> start(start, Mod, Options);
        false -> start(start_link, Mod, Options)
    end.

start(Func, Mod, Options) ->
    gen_server:Func({local, proc_name(Mod)}, ?MODULE, [Mod, Options], []).

cast(Mod, Msg) -> gen_server(cast, Mod, Msg).
call(Mod, Msg) -> gen_server(call, Mod, Msg).

gen_server(Func, Mod, Msg) ->
    Name = proc_name(Mod),
    try gen_server:Func(Name, Msg)
    catch exit:_Reason -> erlang:error({not_mocked, Mod}) end.

proc_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_meck").

original_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_meck_original").

wait_for_exit(Mod) ->
    MonitorRef = erlang:monitor(process, proc_name(Mod)),
    receive {'DOWN', MonitorRef, _Type, _Object, _Info} -> ok end.

unload_if_mocked(P, L) when is_atom(P) ->
    unload_if_mocked(atom_to_list(P), L);
unload_if_mocked(P, L) when length(P) > 5 ->
    case lists:split(length(P) - 5, P) of
        {Name, "_meck"} ->
            Mocked = list_to_existing_atom(Name),
            unload(Mocked),
            [Mocked|L];
        _Else ->
            L
    end;
unload_if_mocked(_P, L) ->
    L.

%% --- Mock handling -----------------------------------------------------------

init_expects(Mod, Options) ->
    case proplists:get_value(passthrough, Options, false) andalso exists(Mod) of
        true -> dict:from_list([{FA, passthrough} || FA <- exports(Mod)]);
        _    -> dict:new()
    end.


get_expect(Expects, Func, Arity) ->
    case e_fetch(Expects, Func, Arity) of
        {sequence, Arity, [Result]} ->
            {{sequence, Arity, Result}, Expects};
        {sequence, Arity, [Result|Rest]} ->
            {{sequence, Arity, Result},
             e_store(Expects, Func, {sequence, Arity, Rest})};
        {loop, Arity, [Result], Loop} ->
            {{loop, Arity, Result},
             e_store(Expects, Func, {loop, Arity, Loop, Loop})};
        {loop, Arity, [Result|Rest], Loop} ->
            {{loop, Arity, Result},
             e_store(Expects, Func, {loop, Arity, Rest, Loop})};
        Other ->
            {Other, Expects}
    end.

store_expect(Mod, Func, Expect, Expects) ->
    change_expects(fun e_store/3, Mod, Func, Expect, Expects).

delete_expect(Mod, Func, Arity, Expects) ->
    change_expects(fun e_delete/3, Mod, Func, Arity, Expects).

change_expects(Op, Mod, Func, Value, Expects) ->
    NewExpects = Op(Expects, Func, Value),
    meck_mod:compile_and_load_forms(to_forms(Mod, NewExpects)),
    NewExpects.

e_store(Expects, Func, Expect) ->
    dict:store({Func, arity(Expect)}, Expect, Expects).

e_fetch(Expects, Func, Arity) ->
    dict:fetch({Func, Arity}, Expects).

e_delete(Expects, Func, Arity) ->
    dict:erase({Func, Arity}, Expects).

%% --- Code generation ---------------------------------------------------------

func(Mod, {Func, Arity}, {anon, Arity, Result}) ->
   case contains_opaque(Result) of
       true ->
            func_exec(Mod, Func, Arity);
       false ->
           func_native(Mod, Func, Arity, Result)
   end;
func(Mod, {Func, Arity}, _Expect) ->
    func_exec(Mod, Func, Arity).

func_exec(Mod, Func, Arity) ->
    Args = args(Arity),
    ?function(Func, Arity,
              [?clause(Args,
                       [?call(?MODULE, exec,
                              [?atom(Mod), ?atom(Func), ?integer(Arity),
                               list(Args)])])]).

func_native(Mod, Func, Arity, Result) ->
    Args = args(Arity),
    AbsResult = erl_parse:abstract(Result),
    ?function(
       Func, Arity,
       [?clause(
           Args,
           [?call(gen_server, cast,
                  [?atom(proc_name(Mod)),
                   ?tuple([?atom(add_history),
                           ?tuple([?tuple([?atom(Mod), ?atom(Func),
                                           list(Args)]),
                                   AbsResult])])]),
            AbsResult])]).

contains_opaque(Term) when is_pid(Term); is_port(Term); is_function(Term) ->
    true;
contains_opaque(Term) when is_list(Term) ->
    lists:any(fun contains_opaque/1, Term);
contains_opaque(Term) when is_tuple(Term) ->
    lists:any(fun contains_opaque/1, tuple_to_list(Term));
contains_opaque(_Term) ->
    false.


to_forms(Mod, Expects) ->
    {Exports, Functions} = functions(Mod, Expects),
    [?attribute(module, Mod)] ++ Exports ++ Functions.

functions(Mod, Expects) ->
    dict:fold(fun(Export, Expect, {Exports, Functions}) ->
                      {[?attribute(export, [Export])|Exports],
                       [func(Mod, Export, Expect)|Functions]}
              end, {[], []}, Expects).

args(0)     -> [];
args(Arity) -> [?var(var_name(N)) || N <- lists:seq(1, Arity)].

list([])    -> {nil, ?LINE};
list([H|T]) -> {cons, ?LINE, H, list(T)}.

var_name(A) -> list_to_atom("A"++integer_to_list(A)).

arity({anon, Arity, _Result}) ->
    Arity;
arity({sequence, Arity, _Sequence}) ->
    Arity;
arity({loop, Arity, _Current, _Loop}) ->
    Arity;
arity(Fun) when is_function(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    Arity.

%% --- Execution utilities -----------------------------------------------------

is_local_function(Fun) ->
    {module, Module} = erlang:fun_info(Fun, module),
    ?MODULE == Module.

handle_mock_exception(Mod, Func, Fun, Args) ->
    case Fun() of
        {exception, Class, Reason} ->
            % exception created with the mock:exception function,
            % do not invalidate Mod
            raise(Mod, Func, Args, Class, Reason);
        {passthrough, PassthroughArgs} ->
            % call_original(Args) called from mock function
            Result = apply(original_name(Mod), Func, PassthroughArgs),
            cast(Mod, {add_history, {{Mod, Func, PassthroughArgs}, Result}}),
            Result
    end.

-spec invalidate_and_raise(_, _, _, _, _) -> no_return().
invalidate_and_raise(Mod, Func, Args, Class, Reason) ->
    call(Mod, invalidate),
    raise(Mod, Func, Args, Class, Reason).

raise(Mod, Func, Args, Class, Reason) ->
    Stacktrace = inject(Mod, Func, Args, erlang:get_stacktrace()),
    cast(Mod, {add_history, {{Mod, Func, Args}, Class, Reason, Stacktrace}}),
    erlang:raise(Class, Reason, Stacktrace).

mock_exception_fun(Class, Reason) -> fun() -> {exception, Class, Reason} end.

passthrough_fun(Args) -> fun() -> {passthrough, Args} end.

call_expect(_Mod, _Func, {_Type, Arity, Return}, VarList)
  when Arity == length(VarList) ->
    Return;
call_expect(Mod, Func, passthrough, VarList) ->
    apply(original_name(Mod), Func, VarList);
call_expect(_Mod, _Func, Fun, VarList) when is_function(Fun) ->
    apply(Fun, VarList).

inject(_Mod, _Func, _Args, []) ->
    [];
inject(Mod, Func, Args, [{meck, exec, _Arity} = Meck|Stack]) ->
    [Meck, {Mod, Func, Args}|Stack];
inject(Mod, Func, Args, [H|Stack]) ->
    [H|inject(Mod, Func, Args, Stack)].

is_mock_exception(Fun) -> is_local_function(Fun).

%% --- Original module handling ------------------------------------------------

backup_original(Module) ->
    Cover = get_cover_state(Module),
    try
        Forms = meck_mod:abstract_code(meck_mod:beam_file(Module)),
        NewName = original_name(Module),
        meck_mod:compile_and_load_forms(meck_mod:rename_module(Forms, NewName),
                                        meck_mod:compile_options(Module))
    catch
        throw:{object_code_not_found, _Module} -> ok; % TODO: What to do here?
        throw:no_abstract_code                 -> ok  % TODO: What to do here?
    end,
    Cover.

restore_original(Mod, false, WasSticky) ->
    restick_original(Mod, WasSticky),
    ok;
restore_original(Mod, {File, Data, Options}, WasSticky) ->
    case filename:extension(File) of
        ".erl" ->
            {ok, Mod} = cover:compile_module(File, Options);
        ".beam" ->
            cover:compile_beam(File)
    end,
    restick_original(Mod, WasSticky),
    ok = cover:import(Data),
    ok = file:delete(Data),
    ok.

unstick_original(Module) -> unstick_original(Module, code:is_sticky(Module)).

unstick_original(Module, true) -> code:unstick_mod(Module);
unstick_original(_,_) -> false.

restick_original(Module, true) ->
    code:stick_mod(Module),
    {module, Module} = code:ensure_loaded(Module),
    ok;
restick_original(_,_) -> ok.

get_cover_state(Module) -> get_cover_state(Module, cover:is_compiled(Module)).

get_cover_state(Module, {file, File}) ->
    Data = atom_to_list(Module) ++ ".coverdata",
    ok = cover:export(Data, Module),
    CompileOptions =
        try
            meck_mod:compile_options(meck_mod:beam_file(Module))
        catch
            throw:{object_code_not_found, _Module} -> []
        end,
    {File, Data, CompileOptions};
get_cover_state(_Module, _IsCompiled) ->
    false.

exists(Module) ->
    code:which(Module) /= non_existing.

exports(Module) ->
    [ FA ||  FA  <- Module:module_info(exports),
             FA /= {module_info, 0}, FA /= {module_info, 1}].

cleanup(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(original_name(Mod)),
    code:delete(original_name(Mod)).

%% --- History utilities -------------------------------------------------------

has_call({_M, _F, _A}, []) -> false;
has_call({M, F, A}, [{{M, F, A}, _Result} | _Rest]) ->
    true;
has_call({M, F, A}, [{{M, F, A}, _ExType, _Exception, _Stack} | _Rest]) ->
    true;
has_call({M, F, A}, [_Call | Rest]) ->
    has_call({M, F, A}, Rest).
