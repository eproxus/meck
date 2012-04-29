-module(meck_test_behaviour_info_module).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{a, 0}].
