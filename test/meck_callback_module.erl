-module(meck_callback_module).
-behaviour(meck_behaviour_module).

-export([ping/0]).

ping() -> ok.
