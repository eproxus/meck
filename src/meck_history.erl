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
-export_type([stack_trace_rec_r14b/0,
              stack_trace_rec_r15b/0,
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

-type stack_trace_rec_r14b() :: {Mod::atom(), Func::atom(),
                                 AriOrArgs::byte() | [any()]}.

-type stack_trace_rec_r15b() :: {Mod::atom(), Func::atom(),
                                 AriOrArgs::byte() | [any()],
                                 Location::[{atom(), any()}]}.

-type stack_trace() :: [stack_trace_rec_r14b() | stack_trace_rec_r15b()].

-type meck_mfa() :: {Mod::atom(), Func::atom(), Args::[term()]}.

-type successfull_call() :: {CallerPid::pid(), meck_mfa(), Result::any()}.

-type faulty_call() :: {CallerPid::pid(), meck_mfa(), Class::exit|error|throw,
                        Reason::term(), stack_trace()}.

-type history_record() :: successfull_call() | faulty_call().
-type history() :: [history_record()].

-type opt_pid() :: pid() | '_'.
-type opt_func() :: atom() | '_'.

%%%============================================================================
%%% API
%%%============================================================================

-spec get_history(opt_pid(), Mod::atom()) -> history().
get_history('_', Mod) ->
    meck_proc:get_history(Mod);
get_history(CallerPid, Mod) ->
    ArgsMatcher = meck_args_matcher:new('_'),
    lists:filter(new_filter(CallerPid, '_', ArgsMatcher),
                 meck_proc:get_history(Mod)).

-spec num_calls(opt_pid(), Mod::atom(), opt_func(),
                meck_args_matcher:opt_args_spec()) ->
        non_neg_integer().
num_calls(CallerPid, Mod, OptFunc, OptArgsSpec) ->
    ArgsMatcher = meck_args_matcher:new(OptArgsSpec),
    Filter = new_filter(CallerPid, OptFunc, ArgsMatcher),
    Filtered = lists:filter(Filter, meck_proc:get_history(Mod)),
    length(Filtered).

%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec new_filter(opt_pid(), opt_func(), meck_args_matcher:args_matcher()) ->
        fun((history_record()) -> boolean()).
new_filter(TheCallerPid, TheFunc, ArgsMatcher) ->
    fun({CallerPid, {_Mod, Func, Args}, _Result})
          when (TheFunc == '_' orelse Func == TheFunc) andalso
               (TheCallerPid == '_' orelse CallerPid == TheCallerPid) ->
            meck_args_matcher:match(Args, ArgsMatcher);
       ({CallerPid, {_Mod, Func, Args}, _Class, _Reason, _StackTrace})
          when (TheFunc == '_' orelse Func == TheFunc) andalso
               (TheCallerPid == '_' orelse CallerPid == TheCallerPid) ->
            meck_args_matcher:match(Args, ArgsMatcher);
       (_Other) ->
            false
    end.
