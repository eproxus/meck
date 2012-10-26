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
-export_type([stack_trace_rec/0,
              stack_trace/0,
              meck_mfa/0,
              successfull_call/0,
              faulty_call/0,
              history/0]).

-export([get_history/2,
         num_calls/4]).

%%%============================================================================
%%% Types
%%%============================================================================

-type stack_trace_rec() :: {Mod::atom(), Func::atom(),
                            AriOrArgs::byte() | [any()],
                            Location::[{atom(), any()}]}.

-type stack_trace() :: [stack_trace_rec()].

-type meck_mfa() :: {Mod::atom(), Func::atom(), Args::[term()]}.

-type successfull_call() :: {CallerPid::pid(), meck_mfa(), Result::any()}.

-type faulty_call() :: {CallerPid::pid(), meck_mfa(), Class::exit|error|throw,
                        Reason::term(), stack_trace()}.

-type history() :: [successfull_call() | faulty_call()].

-type opt_pid() :: pid() | '_'.
-type opt_mod() :: Mod::atom() | '_'.
-type opt_func() :: Func::atom() | '_'.
-type opt_args() :: [any() | '_'] | '_'.


%%%============================================================================
%%% API
%%%============================================================================

-spec get_history(opt_pid(), Mod::atom()) -> history().
get_history('_', Mod) ->
    meck_proc:get_history(Mod);
get_history(CallerPid, Mod) ->
    filter_history(new_filter(CallerPid, '_', '_', '_'), meck_proc:get_history(Mod)).


-spec num_calls(opt_pid(), opt_mod(), opt_func(), opt_args()) ->
        non_neg_integer().
num_calls(CallerPid, Mod, Func, Args) ->
    length(filter_history(new_filter(CallerPid, Mod, Func, Args),
                          meck_proc:get_history(Mod))).


%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec filter_history([meck_util:match_spec_item()], history()) ->
        Selected::history().
filter_history(Filter, History) ->
    MS = ets:match_spec_compile(Filter),
    ets:match_spec_run(History, MS).


-spec new_filter(opt_pid(), opt_mod(), opt_func(), opt_args()) ->
        Filter::[meck_util:match_spec_item()].
new_filter(CallerPid, Mod, Func, Args) ->
    Mfa = {Mod, Func, Args},
    [meck_util:match_spec_item({CallerPid, Mfa, '_'}),
     meck_util:match_spec_item({CallerPid, Mfa, '_', '_', '_'})].
