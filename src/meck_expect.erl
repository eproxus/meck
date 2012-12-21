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
%%% @doc Provides expectation processing functions.
-module(meck_expect).

%% API
-export_type([func_ari/0,
              func_clause/0,
              expect/0]).

-export([new/2,
         new/3,
         new_passthrough/1,
         new_dummy/2,
         match/2]).

%%%============================================================================
%%% Types
%%%============================================================================

-type pattern_matcher() :: {pattern, meck:args_spec(), ets:comp_match_spec()}.

-type hamcrest_matcher() :: {hamcrest, any()}. % TODO define 'any'.

-type args_matcher() :: pattern_matcher() | hamcrest_matcher().

-opaque func_clause() :: {args_matcher(), meck_ret_spec:ret_spec()}.

-type func_ari() :: {Func::atom(), Ari::byte()}.

-type expect() :: {func_ari(), [func_clause()]}.

%%%============================================================================
%%% API
%%%============================================================================

-spec new(Func::atom(), fun() | meck:func_clause_spec()) -> expect().
new(Func, StubFun) when is_function(StubFun) ->
    {arity, Arity} = erlang:fun_info(StubFun, arity),
    Clause = {arity_2_matcher(Arity), meck_ret_spec:exec(StubFun)},
    {{Func, Arity}, [Clause]};
new(Func, ClauseSpecs) when is_list(ClauseSpecs) ->
    {Arity, Clauses} = parse_clause_specs(ClauseSpecs),
    {{Func, Arity}, Clauses}.

-spec new(Func::atom(), byte() | meck:args_spec(), meck_ret_spec:ret_spec()) ->
        expect().
new(Func, Ari, RetSpec) when is_integer(Ari), Ari >= 0 ->
    Clause = {arity_2_matcher(Ari), RetSpec},
    {{Func, Ari}, [Clause]};
new(Func, ArgsSpec, RetSpec) when is_list(ArgsSpec) ->
    {Ari, Clause} = parse_clause_spec({ArgsSpec, RetSpec}),
    {{Func, Ari}, [Clause]}.

-spec new_passthrough(func_ari()) -> expect().
new_passthrough({Func, Ari}) ->
    {{Func, Ari}, [{arity_2_matcher(Ari), meck_passthrough}]}.

-spec new_dummy(func_ari(), meck_ret_spec:ret_spec()) -> expect().
new_dummy({Func, Ari}, RetSpec) ->
    {{Func, Ari}, [{arity_2_matcher(Ari), RetSpec}]}.

-spec match(Args::[any()], Clauses::[func_clause()]) ->
        {undefined, unchanged} |
        {meck_ret_spec:result_spec(), unchanged} |
        {meck_ret_spec:result_spec(), NewClauses::[func_clause()]}.
match(Args, Clauses) ->
    case find_match(Args, Clauses) of
        not_found ->
            {undefined, unchanged};
        {ArgsMatcher, RetSpec} ->
            case meck_ret_spec:retrieve_result(RetSpec) of
                {ResultSpec, unchanged} ->
                    {ResultSpec, Clauses};
                {ResultSpec, NewRetSpec} ->
                    NewClauses = lists:keyreplace(ArgsMatcher, 1, Clauses,
                                                  {ArgsMatcher, NewRetSpec}),
                    {ResultSpec, NewClauses}
            end
    end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec arity_2_matcher(Ari::byte()) -> pattern_matcher().
arity_2_matcher(Ari) ->
    ArgsPattern = lists:duplicate(Ari, '_'),
    MatchSpecItem = meck_util:match_spec_item({ArgsPattern}),
    MatchSpec = ets:match_spec_compile([MatchSpecItem]),
    {pattern, ArgsPattern, MatchSpec}.

-spec parse_clause_specs([meck:func_clause_spec()]) ->
        {Ari::byte(), [func_clause()]}.
parse_clause_specs(ClauseSpecs) ->
    parse_clause_specs(ClauseSpecs, undefined, []).

-spec parse_clause_specs([meck:func_clause_spec()],
                         DeducedAri::byte() | undefined,
                         ParsedClauses::[func_clause()]) ->
        {Ari::byte(), [func_clause()]}.
parse_clause_specs([ClauseSpec | Rest], undefined, []) ->
    {Ari, Clause} = parse_clause_spec(ClauseSpec),
    parse_clause_specs(Rest, Ari, [Clause]);
parse_clause_specs([ClauseSpec | Rest], DeducedAri, Clauses) ->
    {Ari, Clause} = parse_clause_spec(ClauseSpec),
    case Ari of
        DeducedAri ->
            parse_clause_specs(Rest, DeducedAri, [Clause | Clauses]);
        _ ->
            erlang:error({invalid_arity, {{expected, DeducedAri},
                                          {actual, Ari},
                                          {clause, Clause}}})
    end;
parse_clause_specs([], DeducedArity, Clauses) ->
    {DeducedArity, lists:reverse(Clauses)}.

-spec parse_clause_spec(meck:func_clause_spec()) ->
        {Ari::byte(), func_clause()}.
parse_clause_spec({ArgsSpec, RetSpec}) ->
    Ari = length(ArgsSpec),
    MatchSpec = ets:match_spec_compile([meck_util:match_spec_item({ArgsSpec})]),
    Clause = {{pattern, ArgsSpec, MatchSpec}, RetSpec},
    {Ari, Clause}.

-spec find_match(Args::[any()], Defined::[func_clause()]) ->
        Matching:: func_clause() | not_found.
find_match(Args, [{ArgsMatcher, RetSpec} | Rest]) ->
    case ArgsMatcher of
        {pattern, _ArgsPattern, MatchSpec} ->
            case ets:match_spec_run([{Args}], MatchSpec) of
                [] ->
                    find_match(Args, Rest);
                _ ->
                    {ArgsMatcher, RetSpec}
            end
    end;
find_match(_Args, []) ->
    not_found.
