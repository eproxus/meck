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
%%% @doc Contains utility functions that used around other meck modules.
-module(meck_util).

%% API
-export_type([match_spec_item/0]).

-export([proc_name/1]).
-export([original_name/1]).
-export([match_spec_item/1]).

%%%============================================================================
%%% Types
%%%============================================================================

-type match_spec_item() :: {Pattern :: tuple(), Guards :: [any()], Result :: [any()]}.

%%%============================================================================
%%% API
%%%============================================================================

-spec proc_name(Mod :: atom()) -> MockMod :: atom().
proc_name(Name) ->
  list_to_atom(atom_to_list(Name) ++ "_meck").

-spec original_name(Mod :: atom()) -> OrigMod :: atom().
original_name(Name) ->
  list_to_atom(atom_to_list(Name) ++ "_meck_original").

-spec match_spec_item(Pattern :: tuple()) -> match_spec_item().
match_spec_item({ListPattern} = Pattern) when is_list(ListPattern) ->
  case lists:any(fun(Arg) -> is_map(Arg) end, ListPattern) of
    true ->
      match_spec_with_map_item(ListPattern);
    _ ->
      {Pattern, [], ['$_']}
  end;
match_spec_item(Pattern) ->
  {Pattern, [], ['$_']}.

-spec match_spec_with_map_item(ListPattern :: list()) -> match_spec_item().
match_spec_with_map_item(ListPattern) ->
  WithIndex =
    lists:zip(
      lists:seq(0, length(ListPattern) - 1), ListPattern),
  MatchSpecHead =
    lists:map(fun({Index, Arg}) ->
                 case is_map(Arg) of
                   true -> index_to_spec_var(Index);
                   false -> Arg
                 end
              end,
              WithIndex),
  MatchSpecGuards =
    lists:foldl(fun({Index, Arg}, Acc) ->
                   case is_map(Arg) of
                     true ->
                       IndexVar = index_to_spec_var(Index),
                       [{'andalso', {is_map, IndexVar}, {'==', {map_size, IndexVar}, map_size(Arg)}}
                        | Acc];
                     false -> Acc
                   end
                end,
                [],
                WithIndex),
  {{MatchSpecHead}, MatchSpecGuards, ['$_']}.

index_to_spec_var(Index) ->
  List = "$" ++ integer_to_list(Index + 1),
  list_to_atom(List).
