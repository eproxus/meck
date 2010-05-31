%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

%% @doc Module mocking library for Erlang.

-module(meck).
-behaviour(gen_server).

%% Interface exports
-export([new/1]).
-export([new/2]).
-export([expect/3]).
-export([expect/4]).
-export([exception/2]).
-export([passthrough/1]).
-export([history/1]).
-export([validate/1]).
-export([unload/1]).

%% Callback exports
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([exec/4]).

%% Includes
-ifdef(TEST).
-include("test/meck_tests.hrl").
-endif.

%% Types
%% @type meck_mfa() = {Mod::atom(), Func::atom(), Args::list(term())}.
%% Module, function and arguments that the mock module got called with.
-type meck_mfa() :: {Mod::atom(), Func::atom(), Args::list(term())}.

%% @type history() = [{meck_mfa(), Result::term()}
%%                     | {meck_mfa(), Class:: exit | error | throw,
%%                        Reason::term(), Stacktrace::list(mfa())}].
%% History is a list of either successful function calls with a
%% returned result or function calls that resulted in an exception
%% with a type, reason and a stacktrace.
-type history() :: [{meck_mfa(), Result::term()}
                    | {meck_mfa(), Class:: exit | error | throw,
                       Reason::term(), Stacktrace::list(mfa())}].

%% Records
-record(state, {mod :: atom(),
                expects :: dict(),
                valid = true :: boolean(),
                history = [] :: history(),
                original :: term()}).

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @spec new(Mod:: atom() | list(atom())) -> ok
%% @equiv new(Mod, [])
-spec new(Mod::atom()) -> ok.
new(Mod) when is_atom(Mod) ->
    new(Mod, []);
new(Mod) when is_list(Mod) ->
    [new(M) || M <- Mod],
    ok.

%% @spec new(Mod:: atom() | list(atom()), Options::list(term())) -> ok
%% @doc Creates new mocked module(s).
%%
%% This replaces the current version (if any) of the modules in `Mod'
%% with an empty module.
%%
%% Since this library is intended to use from test code, this
%% function links a process for each mock to the calling process.
-spec new(Mod:: atom() | list(atom()), Options::list(term())) -> ok.
new(Mod, Options) when is_atom(Mod), is_list(Options) ->
    case start_link(Mod, Options) of
        {ok, _Pid} -> ok;
        {error, Reason} -> erlang:error(Reason)
    end;
new(Mod, Options) when is_list(Mod) ->
    [new(M, Options) || M <- Mod],
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
-spec expect(Mod:: atom() | list(atom()), Func::atom(), Expect::fun()) -> ok.
expect(Mod, Func, Expect)
  when is_atom(Mod), is_atom(Func), is_function(Expect) ->
    call(Mod, {expect, Func, Expect});
expect(Mod, Func, Expect) when is_list(Mod) ->
    [expect(M, Func, Expect) || M <- Mod],
    ok.

%% @spec expect(Mod:: atom() | list(atom()), Func::atom(),
%%              Arity::pos_integer(), Result::term()) -> ok
%% @doc Adds an expectation with the supplied arity and return value.
%%
%% This creates an expectation which takes `Arity' number of functions
%% and always returns `Result'.
%%
%% @see expect/3.
-spec expect(Mod:: atom() | list(atom()), Func::atom(),
             Arity::pos_integer(), Result::term()) -> ok.
expect(Mod, Func, Arity, Result)
  when is_atom(Mod), is_atom(Func), is_integer(Arity), Arity >= 0 ->
    call(Mod, {expect, Func, Arity, Result});
expect(Mod, Func, Arity, Result) when is_list(Mod) ->
    [expect(M, Func, Arity, Result) || M <- Mod],
    ok.

%% @spec exception(Class:: throw | error | exit, Reason::term()) -> no_return()
%% @doc Throws an expected exception inside an expect fun.
%%
%% This exception will get thrown without invalidating the mocked
%% module. That is, the code using the mocked module is expected to
%% handle this exception.
-spec exception(Class:: throw | error | exit, Reason::term()) -> no_return().
exception(Class, Reason) when Class == throw; Class == error; Class == exit ->
    throw(mock_exception_fun(Class, Reason)).

%% @spec passthrough(Args::list(term())) -> no_return()
%% @doc Calls the original function (if existing) inside an expectation fun.
%%
%% This call does not return, thus everything after this call inside
%% an expectation fun will be ignored.
-spec passthrough(Args::list(term())) -> no_return().
passthrough(Args) ->
    throw(passthrough_fun(Args)).

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
-spec validate(Mod::atom()) -> boolean().
validate(Mod) when is_atom(Mod) ->
    call(Mod, validate);
validate(Mod) when is_list(Mod) ->
    lists:all(fun(true) -> true; (false) -> false end,
              [validate(M) || M <- Mod]).

%% @spec history(Mod::atom()) -> history()
%% @doc Return the call history of the mocked module.
%%
%% Returns a list of calls to the mocked module and their
%% results. Results can be either normal Erlang terms or exceptions
%% that occured.
-spec history(Mod::atom()) -> history().
history(Mod) when is_atom(Mod) ->
    call(Mod, history).

%% @spec unload(Mod:: atom() | list(atom())) -> ok
%% @doc Unload a mocked module or a list of mocked modules.
%%
%% This will purge and delete the module(s) from the Erlang VM. If the
%% mocked module(s) replaced an existing module, this module will still
%% be in the Erlang load path and can be loaded manually or when
%% called.
-spec unload(Mods:: atom() | list(atom())) -> ok.
unload(Mod) when is_atom(Mod) ->
    call(Mod, stop),
    wait_for_exit(Mod);
unload(Mods) when is_list(Mods) ->
    [unload(Mod) || Mod <- Mods],
    ok.

%%==============================================================================
%% Callback functions
%%==============================================================================

%% @hidden
init([Mod, Options]) ->
    Original = backup_original(Mod),
    process_flag(trap_exit, true),
    Expects = init_expects(Mod, Options),
    compile_and_load(to_forms(Mod, Expects)),
    {ok, #state{mod = Mod, expects = Expects, original = Original}}.

%% @hidden
handle_call({get_expect, Func, Arity}, _From, S) ->
    Expect = fetch(S#state.expects, Func, Arity),
    {reply, Expect, S};
handle_call({expect, Func, Expect}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, Expect, S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call({expect, Func, Arity, Result}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, {anon, Arity, Result},
                              S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call(history, _From, S) ->
    {reply, lists:reverse(S#state.history), S};
handle_call({add_history, Item}, _From, S) ->
    {reply, ok, S#state{history = [Item| S#state.history]}};
handle_call(invalidate, _From, S) ->
    {reply, ok, S#state{valid = false}};
handle_call(validate, _From, S) ->
    {reply, S#state.valid, S};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S}.

store_expect(Mod, Func, Expect, Expects) ->
    NewExpects = store(Expects, Func, Expect),
    % only recompile if function was added or arity was changed
    case interface_equal(NewExpects, Expects) of
      true -> ok;
      false -> compile_and_load(to_forms(Mod, NewExpects))
    end,
    NewExpects.

%% @hidden
handle_cast(_Msg, S) ->
    {noreply, S}.

%% @hidden
handle_info(_Info, S) ->
    {noreply, S}.

%% @hidden
terminate(_Reason, #state{mod = Mod, original = OriginalState}) ->
    cleanup(Mod),
    restore_original(Mod, OriginalState),
    ok.

%% @hidden
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%% @hidden
exec(Mod, Func, Arity, Args) ->
    Expect = call(Mod, {get_expect, Func, Arity}),
    try
        Result = call_expect(Mod, Func, Expect, Args),
        call(Mod, {add_history, {{Mod, Func, Args}, Result}}),
        Result
    catch
        throw:Fun when is_function(Fun) ->
            case is_mock_exception(Fun) of
                true ->
                    handle_mock_exception(Mod, Func, Fun, Args);
                false ->
                    invalidate_and_raise(Mod, Func, Args, throw, Fun)
            end;
        Class:Reason ->
            invalidate_and_raise(Mod, Func, Args, Class, Reason)
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

%% --- Process functions -------------------------------------------------------

start_link(Mod, Options) ->
    gen_server:start_link({local, proc_name(Mod)}, ?MODULE, [Mod, Options], []).

call(Mod, Msg) ->
    try gen_server:call(proc_name(Mod), Msg)
    catch exit:{noproc, _Reason} ->
            erlang:error({not_mocked, Mod})
    end.

proc_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_meck").

original_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_meck_original").

init_expects(Mod, Options) ->
    case proplists:get_value(passthrough, Options, false) andalso exists(Mod) of
        true -> dict:from_list([{FA, passthrough} || FA <- exports(Mod)]);
        _    -> dict:new()
    end.

store(Expects, Func, Expect) ->
    dict:store({Func, arity(Expect)}, Expect, Expects).

fetch(Expects, Func, Arity) ->
    dict:fetch({Func, Arity}, Expects).

interface_equal(NewExpects, OldExpects) ->
    dict:fetch_keys(NewExpects) == dict:fetch_keys(OldExpects).

cleanup(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(original_name(Mod)),
    code:delete(original_name(Mod)).

wait_for_exit(Mod) ->
    MonitorRef = erlang:monitor(process, proc_name(Mod)),
    receive {'DOWN', MonitorRef, _Type, _Object, _Info} -> ok end.

%% --- Code generation ---------------------------------------------------------

func(Mod, {Func, Arity}) ->
    Args = args(Arity),
    {function, ?LINE, Func, Arity,
     [{clause, ?LINE, Args, [],
       [{call, ?LINE, {remote, ?LINE, atom(?MODULE), atom(exec)},
         [atom(Mod), atom(Func), integer(Arity), var_list(Args)]}]}]}.

to_forms(Mod, Expects) ->
    {Exports, Functions} = functions(Mod, Expects),
    [attribute(module, Mod)] ++ Exports ++ Functions.

functions(Mod, Expects) ->
    dict:fold(fun(Export, _Expect, {Exports, Functions}) ->
                      {[attribute(export, [Export])|Exports],
                       [func(Mod, Export)|Functions]}
              end, {[], []}, Expects).

compile_and_load(Forms) ->
    {ok, Mod, Beam} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, "", Beam).

args(0) ->
    [];
args(Arity) ->
    [var(var_name(N)) || N<- lists:seq(1, Arity)].

var_list([]) ->
    {nil, ?LINE};
var_list([H|T]) ->
    {cons, ?LINE, H, var_list(T)}.

var(Name) ->
    {var, ?LINE, Name}.

var_name(A) ->
    list_to_atom("A"++integer_to_list(A)).

arity({anon, Arity, _Result}) ->
    Arity;
arity(Fun) when is_function(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    Arity.

attribute(Attribute, Args) ->
    {attribute, ?LINE, Attribute, Args}.

atom(Atom) when is_atom(Atom) ->
    {atom, ?LINE, Atom}.

integer(Integer) when is_integer(Integer) ->
    {integer, ?LINE, Integer}.

%% --- Execution utilities -----------------------------------------------------

is_local_function(Fun) ->
    {module, Module} = erlang:fun_info(Fun, module),
    ?MODULE == Module.

handle_mock_exception(Mod, Func, Fun, Args) ->
    case Fun() of
        % exception created with the mock:exception function, 
        % do not invalidate Mod
        {exception, Class, Reason} ->
            raise(Mod, Func, Args, Class, Reason);
        % call_original(Args) called from mock function
        {passthrough, Args} ->
            Result = apply(original_name(Mod), Func, Args),
            call(Mod, {add_history, {{Mod, Func, Args}, Result}}),
            Result
    end.

-spec invalidate_and_raise(_, _, _, _, _) -> no_return().
invalidate_and_raise(Mod, Func, Args, Class, Reason) ->
    call(Mod, invalidate),
    raise(Mod, Func, Args, Class, Reason).

raise(Mod, Func, Args, Class, Reason) ->
    Stacktrace = inject(Mod, Func, Args, erlang:get_stacktrace()),
    call(Mod, {add_history, {{Mod, Func, Args}, Class, Reason, Stacktrace}}),
    erlang:raise(Class, Reason, Stacktrace).

mock_exception_fun(Class, Reason) ->
    fun() ->
        {exception, Class, Reason}
    end.

passthrough_fun(Args) ->
    fun() ->
        {passthrough, Args}
    end.

call_expect(_Mod, _Func, {anon, Arity, Return}, VarList)
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

is_mock_exception(Fun) ->
    is_local_function(Fun).

%% --- Original module handling ------------------------------------------------

backup_original(Module) ->
    Cover = get_cover_state(Module),
    try
        Forms = abstract_code(beam_file(Module)),
        NewName = original_name(Module),
        case compile:forms(rename_module(Forms, NewName),
                           compile_options(Module)) of
            {ok, NewName, Binary} ->
                load_binary(NewName, Binary);
            {ok, NewName, Binary, []} ->
                load_binary(NewName, Binary);
            {ok, NewName, Binary, Warnings} ->
                io:format(user, "meck:backup_original/1: module: ~p, warnings: ~p~n", [Module, Warnings]),
                load_binary(NewName, Binary)
        end
    catch
        throw:{object_code_not_found, _Module} ->
            ok;
        throw:no_abstract_code ->
            ok
    end,
    Cover.

restore_original(_Mod, false) ->
    ok;
restore_original(Mod, {File, Data}) ->
    {ok, Mod} = cover:compile(File),
    ok = cover:import(Data),
    file:delete(Data),
    ok.

get_cover_state(Module) ->
    get_cover_state(Module, cover:is_compiled(Module)).

get_cover_state(_Module, false) ->
    false;
get_cover_state(Module, {file, File}) ->
    Data = atom_to_list(Module) ++ ".coverdata",
    ok = cover:export(Data, Module),
    {File, Data}.

exists(Module) ->
    code:which(Module) /= non_existing.

exports(Module) ->
    [ FA ||  FA  <- Module:module_info(exports),
             FA /= {module_info, 0},
             FA /= {module_info, 1}].

load_binary(Name, Binary) ->
    case code:load_binary(Name, "", Binary) of
        {module, Name} ->
            ok;
        {error, What} ->
            throw(What) % TODO: Leaks outside the module interface!
    end.

beam_file(Module) ->
    % code:which/1 cannot be used for cover_compiled modules
    case code:get_object_code(Module) of
        {_, Binary, _Filename} -> Binary;
        error                  -> throw({object_code_not_found, Module})
    end.

abstract_code(BeamFile) ->
    case beam_lib:chunks(BeamFile, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms;
        {ok, {_, [{abstract_code, no_abstract_code}]}} ->
            throw(no_abstract_code)
    end.

compile_options(Module) ->
    proplists:get_value(options, Module:module_info(compile)).

rename_module([{attribute, Line, module, _OldName}|T], NewName) ->
    [{attribute, Line, module, NewName}|T];
rename_module([H|T], NewName) ->
    [H|rename_module(T, NewName)].
