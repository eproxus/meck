%%==============================================================================
%% Copyright 2011 Adam Lindberg & Erlang Solutions Ltd.
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

%% @author Adam Lindberg <eproxus@gmail.com>
%% @copyright 2011, Adam Lindberg & Erlang Solutions Ltd
%% @doc Module wrangling helper functions.

-module(meck_mod).

%% Interface exports
-export([abstract_code/1]).
-export([beam_file/1]).
-export([compile_and_load_forms/1]).
-export([compile_and_load_forms/2]).
-export([compile_options/1]).
-export([rename_module/2]).

%% Types
-type erlang_form() :: term().
-type compile_options() :: [term()].

%%==============================================================================
%% Interface exports
%%==============================================================================

-spec abstract_code(binary()) -> erlang_form().
abstract_code(BeamFile) ->
    case beam_lib:chunks(BeamFile, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms;
        {ok, {_, [{abstract_code, no_abstract_code}]}} ->
            throw(no_abstract_code)
    end.

-spec beam_file(module()) -> binary().
beam_file(Module) ->
    % code:which/1 cannot be used for cover_compiled modules
    case code:get_object_code(Module) of
        {_, Binary, _Filename} -> Binary;
        error                  -> throw({object_code_not_found, Module})
    end.

-spec compile_and_load_forms(erlang_form()) -> ok.
compile_and_load_forms(AbsCode) -> compile_and_load_forms(AbsCode, []).

-spec compile_and_load_forms(erlang_form(), compile_options()) -> ok.
compile_and_load_forms(AbsCode, Opts) ->
    case compile:forms(AbsCode, Opts) of
        {ok, ModName, Binary} ->
            load_binary(ModName, Binary);
        {ok, ModName, Binary, _Warnings} ->
            load_binary(ModName, Binary)
    end.

-spec compile_options(binary() | module()) -> compile_options().
compile_options(BeamFile) when is_binary(BeamFile) ->
    case beam_lib:chunks(BeamFile, [compile_info]) of
        {ok, {_, [{compile_info, Info}]}} ->
            proplists:get_value(options, Info);
        _ ->
            []
    end;
compile_options(Module) ->
    proplists:get_value(options, Module:module_info(compile)).

-spec rename_module(erlang_form(), module()) -> erlang_form().
rename_module([{attribute, Line, module, _OldName}|T], NewName) ->
    [{attribute, Line, module, NewName}|T];
rename_module([H|T], NewName) ->
    [H|rename_module(T, NewName)].

%%==============================================================================
%% Internal functions
%%==============================================================================

load_binary(Name, Binary) ->
    case code:load_binary(Name, "", Binary) of
        {module, Name}  -> ok;
        {error, Reason} -> exit({error_loading_module, Name, Reason})
    end.
