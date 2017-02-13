-module(meck_map_type).

-export([foobar/0]).

-type foo() :: #{binary()=>binary()}.

-spec foobar() -> foo().
foobar() -> #{<<"hello">> => <<"world">>}.
