all:
	@./rebar compile

test: force
	@./rebar eunit

force: ;
