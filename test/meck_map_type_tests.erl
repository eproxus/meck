-module(meck_map_type_tests).

-include_lib("eunit/include/eunit.hrl").

map_type_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun mock_map_types/0]}.

setup() ->
    ok.

teardown(_) ->
    meck:unload().

mock_map_types() ->
    ?assertEqual(ok, meck:new(meck_map_type, [])).
