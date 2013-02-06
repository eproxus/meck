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
-module(meck_args_matcher).

-export_type([args_spec/0,
              opt_args_spec/0,
              args_matcher/0]).

%% API
-export([new/1,
         match/2]).

%%%============================================================================
%%% Definitions
%%%============================================================================

-record(args_matcher, {args_spec :: opt_args_spec(),
                       comp_match_spec :: ets:comp_match_spec(),
                       has_matchers = false :: boolean()}).

%%%============================================================================
%%% Types
%%%============================================================================

-type args_spec() :: [any()].
-type opt_args_spec() :: args_spec() | '_'.
-opaque args_matcher() :: #args_matcher{}.

%%%============================================================================
%%% API
%%%============================================================================

-spec new(byte() | opt_args_spec()) -> args_matcher().
new(ArgsSpec = '_') ->
    MatchSpecItem = meck_util:match_spec_item({ArgsSpec}),
    CompMatchSpec = ets:match_spec_compile([MatchSpecItem]),
    #args_matcher{args_spec = ArgsSpec, comp_match_spec = CompMatchSpec};
new(Ari) when is_number(Ari) ->
    ArgsSpec = lists:duplicate(Ari, '_'),
    MatchSpecItem = meck_util:match_spec_item({ArgsSpec}),
    CompMatchSpec = ets:match_spec_compile([MatchSpecItem]),
    #args_matcher{args_spec = ArgsSpec, comp_match_spec = CompMatchSpec};
new(ArgsSpec) when is_list(ArgsSpec) ->
    {HasMatchers, Pattern} = case strip_off_matchers(ArgsSpec) of
                                 unchanged ->
                                     {false, ArgsSpec};
                                 StrippedArgsSpec ->
                                     {true, StrippedArgsSpec}
                             end,
    MatchSpecItem = meck_util:match_spec_item({Pattern}),
    CompMatchSpec = ets:match_spec_compile([MatchSpecItem]),
    #args_matcher{args_spec = ArgsSpec,
                  comp_match_spec = CompMatchSpec,
                  has_matchers = HasMatchers}.

-spec match(Args::any(), args_matcher()) -> boolean().
match(Args, #args_matcher{args_spec = ArgsSpec,
                          comp_match_spec = CompMatchSpec,
                          has_matchers = HasMatchers}) ->
    case ets:match_spec_run([{Args}], CompMatchSpec) of
        [] ->
            false;
        _Matches when HasMatchers ->
            check_by_matchers(Args, ArgsSpec);
        _Matches ->
            true
    end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec strip_off_matchers(args_spec()) ->
    NewArgsSpec::args_spec() | unchanged.
strip_off_matchers(ArgsSpec) ->
    strip_off_matchers(ArgsSpec, [], false).

-spec strip_off_matchers(args_spec(), Stripped::[any() | '_'], boolean()) ->
    NewArgsSpec::args_spec() | unchanged.
strip_off_matchers([ArgSpec | Rest], Stripped, HasMatchers) ->
    case meck_matcher:is_matcher(ArgSpec) of
        true ->
            strip_off_matchers(Rest, ['_' | Stripped], true);
        _ ->
            strip_off_matchers(Rest, [ArgSpec | Stripped], HasMatchers)
    end;
strip_off_matchers([], Stripped, true) ->
    lists:reverse(Stripped);
strip_off_matchers([], _Stripped, false) ->
    unchanged.

-spec check_by_matchers(Args ::[any()], MaybeMatchers::[any()]) -> boolean().
check_by_matchers([Arg | RestArgs], [MaybeMatcher | RestMaybeMatchers]) ->
    case meck_matcher:match_ignore(Arg, MaybeMatcher) of
        true ->
            check_by_matchers(RestArgs, RestMaybeMatchers);
        _Else ->
            false
    end;
check_by_matchers([], []) ->
    true.