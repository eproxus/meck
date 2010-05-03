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
-export([expect/3]).
-export([unload/1]).
-export([validate/1]).

%% Callback exports
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([exec/4]).
-export([exception/2]).

%% Includes
-ifdef(TEST).
-include("test/meck_tests.hrl").
-endif.

%% Records
-record(state, {mod, expects = dict:new(), valid = true}).

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @doc Create new mocked module.
%%
%% This replaces the current version (if any) of `Mod' with an empty module.
-spec new(Mod :: atom()) -> ok.
new(Mod) when is_atom(Mod) ->
    case start_link(Mod) of
        {ok, _Pid} -> ok;
        {error, Reason} -> erlang:error(Reason)
    end.

%% @doc Add expectation for a function to the module `Mod'.
%%
%% An expectation is a fun that is executed whenever the function
%% `Func' is executed.
%%
%% It also affects the validation status of the module. If an
%% expectation is called with the wrong number of arguments or the
%% wrong arguments the mock module is invalidated. It is also
%% invalidated if an unexpected exception occurs.
%%
%% Since this library is intended to use from test code, this
%% function links a process to the calling process.
%%
%% @see validate/1.
-spec expect(Mod :: atom(), Func :: atom(), Expect :: fun()) -> ok.
expect(Mod, Func, Expect)
  when is_atom(Mod), is_atom(Func), is_function(Expect) ->
    call(Mod, {expect, Func, Expect}).

%% @doc Throws an expected exception inside an expect fun.
%%
%% This exception will get thrown without invalidating the mocked
%% module. That is, the code using the mocked module is expected to
%% handle this exception.
exception(Class, Reason) when Class == throw; Class == error; Class == exit ->
    throw(mock_exception(Class, Reason)).

%% @doc Validate the state of the mock module.
%%
%% The function returns `true' if the mocked module has been used
%% according to its expectations. It returns `false' if a call has
%% failed in some way. Reasons for failure are wrong number of
%% arguments or non-existing function (undef), wrong arguments
%% (function clause)
-spec validate(Mod :: atom()) -> boolean().
validate(Mod) when is_atom(Mod) ->
    call(Mod, validate).

%% @doc Unload the mocked module.
%%
%% This will purge and delete the module from the Erlang VM. If the
%% mocked module replaced an existing module, this module will still
%% be in the Erlang load path and can be loaded manually or when
%% called.
-spec unload(Mod :: atom()) -> ok.
unload(Mod) when is_atom(Mod) ->
    call(Mod, stop),
    code:purge(Mod),
    code:delete(Mod),
    ok.

%%==============================================================================
%% Callback functions
%%==============================================================================

%% @hidden
init([Mod]) ->
    process_flag(trap_exit, true),
    S = #state{mod = Mod},
    compile_and_load(to_forms(S)),
    {ok, S}.

%% @hidden
handle_call({exec, Func, Arity}, _From, S) ->
    Expect = fetch(S#state.expects, Func, Arity),
    {reply, Expect, S};
handle_call({expect, Func, Expect}, _From, S) ->
    NewS = S#state{expects = store(S#state.expects, Func, Expect)},
    % only recompile if function was added
    case interface_equal(NewS#state.expects, S#state.expects) of
        true ->
            ok;
        false ->
            compile_and_load(to_forms(NewS))
    end,
    {reply, ok, NewS};
handle_call(invalidate, _From, S) ->
    {reply, ok, S#state{valid = false}};
handle_call(validate, _From, S) ->
    {reply, S#state.valid, S};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S}.

%% @hidden
handle_cast(_Msg, S) ->
    {noreply, S}.

%% @hidden
handle_info(_Info, S) ->
    {noreply, S}.

%% @hidden
terminate(_Reason, #state{mod=Mod}) ->
    code:purge(Mod),
    code:delete(Mod),
    ok.

%% @hidden
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%% @hidden
exec(Mod, Func, Arity, VarList) ->
    Expect = call(Mod, {exec, Func, Arity}),
    try 
        apply(Expect, VarList)
    catch
        throw:Fun when is_function(Fun) ->
            case is_mock_exception(Fun) of
                true ->
                    % exception created with the mock:exception function,
                    % do not invalidate Mod
                    {Class, Reason} = Fun(),
                    erlang:raise(Class, Reason, erlang:get_stacktrace());
                false ->
                    call(Mod, invalidate),
                    erlang:raise(throw, Fun, erlang:get_stacktrace())
            end;
        Class:Reason ->
            call(Mod, invalidate),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.

%% @hidden
is_mock_exception(Fun) ->
    is_local_function(Fun).

mock_exception(Class, Reason) ->
    fun() ->
            {Class, Reason}
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

start_link(Mod) ->
    gen_server:start_link({local, salt(Mod)}, ?MODULE, [Mod], []).

call(Mod, Msg) ->
    try gen_server:call(salt(Mod), Msg)
    catch exit:{noproc, _Reason} ->
            erlang:error({not_mocked, Mod})
    end.

func(Mod, {Func, Arity}) ->
    Args = args(Arity),
    {function, ?LINE, Func, Arity,
     [{clause, ?LINE, Args, [],
       [{call, ?LINE, {remote, ?LINE, atom(?MODULE), atom(exec)}, 
         [atom(Mod), atom(Func), integer(Arity), var_list(Args)]}]}]}.

to_forms(#state{mod = Mod, expects = Expects}) ->
    {Exports, Functions} = functions(Mod, Expects),
    [attribute(module, Mod)] ++ Exports ++ Functions.

compile_and_load(Forms) ->
    %?debugFmt("\n~p\n\n", [Forms]),
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

functions(Mod, Expects) ->
    dict:fold(fun(Export, _Expect, {Exports, Functions}) ->
                      {[attribute(export, [Export])|Exports],
                       [func(Mod, Export)|Functions]}
              end, {[], []}, Expects).

arity(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    Arity.

salt(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_meck").

store(Expects, Func, Expect) ->
    dict:store({Func, arity(Expect)}, Expect, Expects).

fetch(Expects, Func, Arity) ->
    dict:fetch({Func, Arity}, Expects).

is_local_function(Fun) ->
    {module, Module} = erlang:fun_info(Fun, module),
    ?MODULE == Module.

interface_equal(NewExpects, OldExpects) ->
    length(dict:fetch_keys(NewExpects)) ==
        length(dict:fetch_keys(OldExpects)).


attribute(Attribute, Args) ->
    {attribute, ?LINE, Attribute, Args}.

atom(Atom) when is_atom(Atom) ->
    {atom, ?LINE, Atom}.

integer(Integer) when is_integer(Integer) ->
    {integer, ?LINE, Integer}.
