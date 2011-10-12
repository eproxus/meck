-include_lib("eunit/include/eunit.hrl").

-define(assertWasCalled(Mod,Func,Args),
    ((fun () ->
        case (meck:called(Mod,Func,Args)) of
            true -> ok;
            false ->
                .erlang:error({assertionFailed_mockFunctionNotCalled,
                    [{module, ?MODULE},
                        {line, ?LINE},
                        {functon, {Mod, Func, length(Args)}},
                        {expected_args, Args},
                        {value, not_called}]});
            {wrong_args, ActualArgs} ->
                .erlang:error({assertionFailed_mockFunctionCalledWithWrongArgs,
                    [{module, ?MODULE},
                        {line, ?LINE},
                        {function, {Mod, Func, length(Args)}},
                        {expected_args, Args},
                        {value, ActualArgs}]})
        end
    end)())).
