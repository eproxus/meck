%%%============================================================================
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

%%% @private
%%% @doc Implements code generation for mocked module and also contains code
%%% pieces that are called in the generated module context.
-module(meck_code_gen).

%% API
-export([to_forms/2,
         get_current_call/0,
         exception_fun/2]).

%% Exported to be accessible from generated modules.
-export([exec/4]).


%%%============================================================================
%%% Definitions
%%%============================================================================

-define(CURRENT_CALL, '$meck_call').

-define(call(Module, Function, Arguments),
        {call, ?LINE,
         {remote, ?LINE, ?atom(Module), ?atom(Function)},
         Arguments}).

-define(atom(Atom), {atom, ?LINE, Atom}).

-define(integer(Integer), {integer, ?LINE, Integer}).

-define(var(Name), {var, ?LINE, Name}).

-define(attribute(Attribute, Args), {attribute, ?LINE, Attribute, Args}).

-define(function(Name, Arity, Clauses),
        {function, ?LINE, Name, Arity, Clauses}).

-define(clause(Arguments, Body), {clause, ?LINE, Arguments, [], Body}).

-define(tuple(Elements), {tuple, ?LINE, Elements}).


%%%============================================================================
%%% API
%%%============================================================================

to_forms(Mod, Expects) ->
    {Exports, Functions} = functions(Mod, Expects),
    [?attribute(module, Mod)] ++ attributes(Mod) ++ Exports ++ Functions.


-spec get_current_call() -> {Mod::atom(), Func::atom()}.
get_current_call() ->
    get(?CURRENT_CALL).


%%%============================================================================
%%% Internal functions
%%%============================================================================

attributes(Mod) ->
    try
        [?attribute(Key, Val) || {Key, Val} <-
            proplists:get_value(attributes, Mod:module_info(), []),
            Key =/= vsn]
    catch
        error:undef -> []
    end.


functions(Mod, Expects) ->
    dict:fold(fun(Export, Expect, {Exports, Functions}) ->
        {[?attribute(export, [Export]) | Exports],
         [func(Mod, Export, Expect) | Functions]}
              end,
              {[], []},
              Expects).


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
                              [?call(erlang, self, []),
                               ?atom(Mod),
                               ?atom(Func),
                               list(Args)])])]).


func_native(Mod, Func, Arity, Result) ->
    Args = args(Arity),
    AbsResult = erl_parse:abstract(Result),
    ?function(
            Func, Arity,
            [?clause(
                    Args,
                    [?call(gen_server, cast,
                           [?atom(meck_util:proc_name(Mod)),
                            ?tuple([?atom(add_history),
                                    ?tuple([?call(erlang, self, []),
                                            ?tuple([?atom(Mod), ?atom(Func),
                                                    list(Args)]),
                                            AbsResult])])]),
                     AbsResult])]).


contains_opaque(Term) when is_pid(Term); is_port(Term); is_function(Term);
    is_reference(Term) ->
    true;
contains_opaque(Term) when is_list(Term) ->
    lists:any(fun contains_opaque/1, Term);
contains_opaque(Term) when is_tuple(Term) ->
    lists:any(fun contains_opaque/1, tuple_to_list(Term));
contains_opaque(_Term) ->
    false.


args(0)     -> [];
args(Arity) -> [?var(var_name(N)) || N <- lists:seq(1, Arity)].


list([])    -> {nil, ?LINE};
list([H|T]) -> {cons, ?LINE, H, list(T)}.


var_name(A) -> list_to_atom("A"++integer_to_list(A)).


%% @hidden
-spec exec(CallerPid::pid(), Mod::atom(), Func::atom(), Args::[any()]) ->
        Result::any().
exec(Pid, Mod, Func, Args) ->
    RetSpec = meck_proc:get_ret_spec(Mod, Func, Args),
    try
        put(?CURRENT_CALL, {Mod, Func}),
        Result = simulate_call(Mod, Func, Args, RetSpec),
        meck_proc:add_history(Mod, Pid, Func, Args, Result),
        Result
    catch
        throw:Fun when is_function(Fun) ->
            case is_mock_exception(Fun) of
                true  -> handle_mock_exception(Pid, Mod, Func, Fun, Args);
                false -> invalidate_and_raise(Pid, Mod, Func, Args, throw, Fun)
            end;
        Class:Reason ->
            invalidate_and_raise(Pid, Mod, Func, Args, Class, Reason)
    after
        erase('$meck_call')
    end.


-spec simulate_call(Mod::atom(), Func::atom(), Args::[any()],
                    meck:ret_spec() | meck_undefined) ->
        Result::any().
simulate_call(_Mod, _Func, _Args, meck_undefined) ->
    erlang:error(function_clause);
simulate_call(_Mod, _Func, _Args, {meck_value, Value}) ->
    Value;
simulate_call(_Mod, _Func, Args, {meck_func, Fun}) when is_function(Fun) ->
    apply(Fun, Args);
simulate_call(_Mod, _Func, _Args, {meck_raise, Class, Reason}) ->
    meck:exception(Class, Reason);
simulate_call(Mod, Func, Args, meck_passthrough) ->
    apply(meck_util:original_name(Mod), Func, Args);
simulate_call(_Mod, _Func, _Args, Value) ->
    Value.


-spec handle_mock_exception(CallerPid::pid(), Mod::atom(), Func::atom(),
                            Body::fun(), Args::[any()]) ->
        no_return().
handle_mock_exception(Pid, Mod, Func, Fun, Args) ->
    case Fun() of
        {exception, Class, Reason} ->
            % exception created with the mock:exception function,
            % do not invalidate Mod
            raise(Pid, Mod, Func, Args, Class, Reason)
    end.


-spec invalidate_and_raise(CallerPid::pid(), Mod::atom(), Func::atom(),
                           Args::[any()], Class:: exit | error | throw,
                           Reason::any()) ->
        no_return().
invalidate_and_raise(Pid, Mod, Func, Args, Class, Reason) ->
    meck_proc:invalidate(Mod),
    raise(Pid, Mod, Func, Args, Class, Reason).


-spec raise(CallerPid::pid(), Mod::atom(), Func::atom(), Args::[any()],
            Class:: exit | error | throw, Reason::any()) ->
        no_return().
raise(Pid, Mod, Func, Args, Class, Reason) ->
    StackTrace = inject(Mod, Func, Args, erlang:get_stacktrace()),
    meck_proc:add_history(Mod, Pid, Func, Args, {Class, Reason, StackTrace}),
    erlang:raise(Class, Reason, StackTrace).


-spec exception_fun(Class:: exit | error | throw, Reason::any()) -> fun().
exception_fun(Class, Reason) -> fun() -> {exception, Class, Reason} end.


-spec is_mock_exception(Fun::fun()) -> boolean().
is_mock_exception(Fun) ->
    {module, Mod} = erlang:fun_info(Fun, module),
    ?MODULE == Mod.


-spec inject(Mod::atom(), Func::atom(), Args::[any()],
             meck_history:stack_trace()) ->
        NewStackTrace::meck_history:stack_trace().
inject(_Mod, _Func, _Args, []) ->
    [];
inject(Mod, Func, Args, [{?MODULE, exec, _AriOrArgs, _Loc} = Meck | Stack]) ->
    [Meck, {Mod, Func, Args} | Stack];
inject(Mod, Func, Args, [H | Stack]) ->
    [H | inject(Mod, Func, Args, Stack)].
