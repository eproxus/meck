-module(meck_on_load_tests).

-include_lib("eunit/include/eunit.hrl").

on_load_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun no_enable_on_load/0,
      fun enable_on_load/0]}.

setup() ->
    ok.

teardown(_) ->
    meck:unload().

no_enable_on_load() ->
    % We _don't_ want on_load to be called. Listen out for it.
    register(on_load_listener, self()),
    meck:new(meck_on_load_module, [passthrough]),
    ?assertEqual(pong, meck_on_load_module:ping()),
    receive
        on_load_called -> erlang:error(unexpected_call_to_on_load)
    after 100 ->
              % Use a relatively short timeout, because the happy path goes
              % through here.
              ok
    end.

enable_on_load() ->
    % We _do_ want on_load to be called.
    register(on_load_listener, self()),
    meck:new(meck_on_load_module, [passthrough, enable_on_load]),
    ?assertEqual(pong, meck_on_load_module:ping()),
    receive
        on_load_called -> ok
    after 200 ->
              % Use a longer timeout, because testing for not-called is harder,
              % and this is the sad path.
              erlang:error(expected_call_to_on_load)
    end.
