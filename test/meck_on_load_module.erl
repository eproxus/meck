-module(meck_on_load_module).
-on_load(on_load/0).
-export([ping/0]).

on_load() ->
    % Assumes that there's an on_load_listener.
    catch (on_load_listener ! on_load_called),
    ok.

ping() -> pong.
