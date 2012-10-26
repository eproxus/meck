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

-opaque func_clause() :: {args_matcher(), meck:ret_spec()}.

-type func_ari() :: {Func::atom(), Ari::byte()}.

-type expect() :: {func_ari(), [func_clause()]}.


%%%============================================================================
%%% API
%%%============================================================================

-spec new(Func::atom(), fun() | meck:func_clause_spec()) -> expect().
new(Func, StubFun) when is_function(StubFun) ->
    {arity, Arity} = erlang:fun_info(StubFun, arity),
    Clause = {arity_2_matcher(Arity), {meck_func, StubFun}},
    {{Func, Arity}, [Clause]};
new(Func, ClauseSpecs) when is_list(ClauseSpecs) ->
    {Arity, Clauses} = parse_clause_specs(ClauseSpecs),
    {{Func, Arity}, Clauses}.


-spec new(Func::atom(), byte() | meck:args_spec(), meck:ret_spec()) -> expect().
new(Func, Ari, RetSpec) when is_integer(Ari), Ari >= 0 ->
    Clause = {arity_2_matcher(Ari), RetSpec},
    {{Func, Ari}, [Clause]};
new(Func, ArgsSpec, RetSpec) when is_list(ArgsSpec) ->
    {Ari, Clause} = parse_clause_spec({ArgsSpec, RetSpec}),
    {{Func, Ari}, [Clause]}.


-spec new_passthrough(func_ari()) -> expect().
new_passthrough({Func, Ari}) ->
    {{Func, Ari}, [{arity_2_matcher(Ari), meck_passthrough}]}.


-spec new_dummy(func_ari(), meck:ret_spec()) -> expect().
new_dummy({Func, Ari}, RetSpec) ->
    {{Func, Ari}, [{arity_2_matcher(Ari), RetSpec}]}.


-spec match(Args::[any()], Clauses::[func_clause()]) ->
        {meck_undefined, unchanged} |
        {meck:ret_spec(), unchanged} |
        {meck:ret_spec(), NewClauses::[func_clause()]}.
match(Args, Clauses) ->
    case find_match(Args, Clauses) of
        not_found ->
            {meck_undefined, unchanged};
        {ArgsMatcher, RetSpec} ->
            case next_result(RetSpec, []) of
                {ScalarRs, unchanged} ->
                    {ScalarRs, Clauses};
                {ScalarRs, NewRetSpec} ->
                    NewClauses = lists:keyreplace(ArgsMatcher, 1, Clauses,
                                                  {ArgsMatcher, NewRetSpec}),
                    {ScalarRs, NewClauses}
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


-spec next_result(RetSpec::meck:ret_spec(), Stack::[meck:ret_spec()]) ->
        {ScalarRs::meck:ret_spec(), NewRetSpec::meck:ret_spec() | unchanged}.
next_result(RetSpec = {meck_seq, [InnerRs | _Rest]}, Stack) ->
    next_result(InnerRs, [RetSpec | Stack]);
next_result(RetSpec = {meck_loop, [InnerRs | _Rest], _Loop}, Stack) ->
    next_result(InnerRs, [RetSpec | Stack]);
next_result(LeafRetSpec, Stack) ->
    {LeafRetSpec, unwind_stack(LeafRetSpec, Stack, false)}.


-spec unwind_stack(InnerRs::meck:ret_spec(),
                   Stack::[meck:ret_spec()], Done::boolean()) ->
        NewRetSpec::meck:ret_spec() | unchanged.
unwind_stack(InnerRs, [], true) ->
    InnerRs;
unwind_stack(_InnerRs, [], false) ->
    unchanged;
unwind_stack(InnerRs, [CurrRs = {meck_seq, [InnerRs]} | Stack], Updated) ->
    unwind_stack(CurrRs, Stack, Updated);
unwind_stack(InnerRs, [{meck_seq, [InnerRs | Rest]} | Stack], _Updated) ->
    unwind_stack({meck_seq, Rest}, Stack, true);
unwind_stack(NewInnerRs, [{meck_seq, [_InnerRs | Rest]} | Stack], _Updated) ->
    unwind_stack({meck_seq, [NewInnerRs | Rest]}, Stack, true);
unwind_stack(InnerRs, [{meck_loop, [InnerRs], Loop} | Stack], _Updated) ->
    unwind_stack({meck_loop, Loop, Loop}, Stack, true);
unwind_stack(InnerRs, [{meck_loop, [InnerRs | Rest], Loop} | Stack],
             _Updated) ->
    unwind_stack({meck_loop, Rest, Loop}, Stack, true);
unwind_stack(NewInnerRs, [{meck_loop, [_InnerRs | Rest], Loop} | Stack],
             _Updated) ->
    unwind_stack({meck_loop, [NewInnerRs | Rest], Loop}, Stack, true).
