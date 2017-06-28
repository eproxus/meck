-module(meck_behaviour_module).

-export([pong/0]).

-callback ping() -> ok.

pong() -> ok.
