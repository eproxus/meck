-include_lib("eunit/include/eunit.hrl").

functions_test() ->
    ?assertEqual({[], []}, functions(dummy, dict:new())),
    {Exports, Functions} = functions(dummy, 
                                     dict:from_list([{{test, 0}, undefined}])),
    ?assertMatch([{attribute, _, export, [{test, 0}]}], Exports),
    ?assertEqual(length(Exports), length(Functions)).

