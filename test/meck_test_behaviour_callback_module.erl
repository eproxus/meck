-module(meck_test_behaviour_callback_module).

-export([a/0]).

-behaviour(meck_test_behaviour_info_module).

a() -> ok.
