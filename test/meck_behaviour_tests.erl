-module(meck_behaviour_tests).

-include_lib("eunit/include/eunit.hrl").

mock_behaviour_module_test_() ->
    {
        setup,
        fun() -> catch meck:new([meck_behaviour_module, meck_callback_module], [no_link]) end,
        fun(_) -> meck:unload() end,
        fun(MeckNewResult) -> { "Behaviour modules and callbacks can be mocked together",
            ?_test(begin
                       ?assertEqual(ok, MeckNewResult),

                        meck:expect(meck_callback_module, ping, fun() -> not_ok end),
                        meck:expect(meck_behaviour_module, pong, fun() -> this_is_new end),

                        ?assertEqual(not_ok, meck_callback_module:ping()),
                        ?assertEqual(this_is_new, meck_behaviour_module:pong())
                   end)}
        end
    }.
