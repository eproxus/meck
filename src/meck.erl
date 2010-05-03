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
-export([exception/2]).
-export([passthrough/1]).
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

%% Records
-record(state, {mod, expects = dict:new(), valid = true}).

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @spec new(Mod::atom()) -> ok
%% @equiv new(Mod, [])
-spec new(Mod::atom()) -> ok.
new(Mod) when is_atom(Mod) ->
    new(Mod, []).

%% @spec new(Mod::atom(), Options::list(term())) -> ok
%% @doc Create new mocked module.
%%
%% This replaces the current version (if any) of `Mod' with an empty module.
%%
%% Since this library is intended to use from test code, this
%% function links a process to the calling process.
-spec new(Mod::atom(), Options::list(term())) -> ok.
new(Mod, Options) when is_atom(Mod), is_list(Options) ->
    case start_link(Mod, Options) of
        {ok, _Pid} -> ok;
        {error, Reason} -> erlang:error(Reason)
    end.

%% @spec expect(Mod::atom(), Func::atom(), Expect::fun()) -> ok
%% @doc Add expectation for a function `Func' to the mocked module `Mod'.
%%
%% An expectation is a fun that is executed whenever the function
%% `Func' is called.
%%
%% It affects the validation status of the mocked module. If an
%% expectation is called with the wrong number of arguments or invalid
%% arguments the mock module is invalidated. It is also invalidated if
%% an unexpected exception occurs.
%%
%% @see validate/1.
-spec expect(Mod::atom(), Func::atom(), Expect::fun()) -> ok.
expect(Mod, Func, Expect)
  when is_atom(Mod), is_atom(Func), is_function(Expect) ->
    call(Mod, {expect, Func, Expect}).

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

%% @spec validate(Mod::atom()) -> boolean()
%% @doc Validate the state of the mock module.
%%
%% The function returns `true' if the mocked module has been used
%% according to its expectations. It returns `false' if a call has
%% failed in some way. Reasons for failure are wrong number of
%% arguments or non-existing function (undef), wrong arguments
%% (function clause) or unexpected exceptions.
-spec validate(Mod::atom()) -> boolean().
validate(Mod) when is_atom(Mod) ->
    call(Mod, validate).

%% @spec unload(Mod::atom()) -> ok
%% @doc Unload the mocked module.
%%
%% This will purge and delete the module from the Erlang VM. If the
%% mocked module replaced an existing module, this module will still
%% be in the Erlang load path and can be loaded manually or when
%% called.
-spec unload(Mod::atom()) -> ok.
unload(Mod) when is_atom(Mod) ->
    call(Mod, stop),
    wait_for_exit(Mod).

%%==============================================================================
%% Callback functions
%%==============================================================================

%% @hidden
init([Mod, Options]) ->
    rename_original(Mod),
    process_flag(trap_exit, true),
    S = #state{mod = Mod, expects = init_expects(Mod, Options)},
    compile_and_load(to_forms(S)),
    {ok, S}.

%% @hidden
handle_call({get_expect, Func, Arity}, _From, S) ->
    Expect = fetch(S#state.expects, Func, Arity),
    {reply, Expect, S};
handle_call({expect, Func, Expect}, _From, S) ->
    NewS = S#state{expects = store(S#state.expects, Func, Expect)},
    % only recompile if function was added
    case interface_equal(NewS#state.expects, S#state.expects) of
        true  -> ok;
        false -> compile_and_load(to_forms(NewS))
    end,
    {reply, ok, NewS};
handle_call(invalidate, _From, S) ->
    {reply, ok, S#state{valid = false}};
handle_call(validate, _From, S) ->
    {reply, S#state.valid, S};
handle_call(stop, _From, #state{mod = Mod} = S) ->
    cleanup(Mod),
    {stop, normal, ok, S}.

%% @hidden
handle_cast(_Msg, S) ->
    {noreply, S}.

%% @hidden
handle_info(_Info, S) ->
    {noreply, S}.

%% @hidden
terminate(_Reason, #state{mod = Mod}) ->
    cleanup(Mod),
    ok.

%% @hidden
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%% @hidden
exec(Mod, Func, Arity, Args) ->
    Expect = call(Mod, {get_expect, Func, Arity}),
    try
        call_expect(Mod, Func, Expect, Args)
    catch
        throw:Fun when is_function(Fun) ->
            case is_mock_exception(Fun) of
                true ->
                    handle_mock_exception(Mod, Func, Fun, Args);
                false ->
                    call(Mod, invalidate),
                    Stacktrace = inject(Mod, Func, Args,
                                        erlang:get_stacktrace()),
                    erlang:raise(throw, Fun, Stacktrace)
            end;
        Class:Reason ->
            call(Mod, invalidate),
            Stacktrace = inject(Mod, Func, Args, erlang:get_stacktrace()),
            erlang:raise(Class, Reason, Stacktrace)
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
    length(dict:fetch_keys(NewExpects)) ==
        length(dict:fetch_keys(OldExpects)).

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

to_forms(#state{mod = Mod, expects = Expects}) ->
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

arity(Fun) ->
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
            erlang:raise(Class, Reason,
                         inject(Mod, Func, Args, erlang:get_stacktrace()));
        % call_original(Args) called from mock function
        {passthrough, Args} ->
            apply(original_name(Mod), Func, Args)
    end.

mock_exception_fun(Class, Reason) ->
    fun() ->
            {exception, Class, Reason}
    end.

passthrough_fun(Args) ->
    fun() ->
            {passthrough, Args}
    end.

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

rename_original(Module) ->
    try
        Forms = abstract_code(beam_file(Module)),
        NewName = original_name(Module),
        {ok, NewName, Binary} =
        compile:forms(rename_module(Forms, NewName),
                      compile_options(Module)),
        load_binary(NewName, Binary)
    catch
        throw:{object_code_not_found, _Module} ->
            ok;
        throw:no_abstract_code ->
            ok
    end.

exists(Module) ->
    code:which(Module) /= non_existing.

exports(Module) ->
    [ FA ||  {F, A}=FA  <- Module:module_info(exports),
             (F /= module_info) and ((A == 1) or (A == 0))].

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
