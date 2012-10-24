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
%%% @doc Provides functions for digging information from the recorded call
%%% history.
-module(meck_history).

%% API
-export([get_history/2,
         num_calls/4]).

%%%============================================================================
%%% Types
%%%============================================================================

-type opt_pid() :: pid() | '_'.
-type opt_mod() :: Mod::atom() | '_'.
-type opt_func() :: Func::atom() | '_'.
-type opt_arg() :: any() | '_'.
-type opt_args() :: [opt_arg()] | '_'.


%%%============================================================================
%%% API
%%%============================================================================

-spec get_history(opt_pid(), Mod::atom()) -> meck:history().
get_history('_', Mod) ->
    meck_proc:get_history(Mod);
get_history(Pid, Mod) ->
    filter_history(new_filter(Pid, '_', '_', '_'), meck_proc:get_history(Mod)).


-spec num_calls(opt_pid(), opt_mod(), opt_func(), opt_args()) ->
        non_neg_integer().
num_calls(Pid, Mod, Func, Args) ->
    length(filter_history(new_filter(Pid, Mod, Func, Args),
                          meck_proc:get_history(Mod))).


%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec filter_history([meck_util:match_spec_item()], meck:history()) ->
        Selected::meck:history().
filter_history(Filter, History) ->
    MS = ets:match_spec_compile(Filter),
    ets:match_spec_run(History, MS).


-spec new_filter(opt_pid(), opt_mod(), opt_func(), opt_args()) ->
        Filter::[meck_util:match_spec_item()].
new_filter(Pid, Mod, Func, Args) ->
    Mfa = {Mod, Func, Args},
    [meck_util:match_spec_item({Pid, Mfa, '_'}),
     meck_util:match_spec_item({Pid, Mfa, '_', '_', '_'})].
