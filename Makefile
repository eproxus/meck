PLTFILE=$(CURDIR)/.deps.plt
APP_DEPS=kernel stdlib eunit tools compiler
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

REBAR=$(shell which rebar)
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

ERL = $(shell which erl)
ifeq ($(ERL),)
$(error "Erlang must be available on this system")
endif

.PHONY: all rebuild compile doc clean test dialyzer typer \
	shell clean-plt clean-doc distclean

all: compile dialyzer test doc

rebuild: clean-doc clean all

compile:
	@$(REBAR) skip_deps=true compile

doc:
	@$(REBAR) skip_deps=true doc

clean:
	@$(REBAR) skip_deps=true clean

test:
	@$(REBAR) skip_deps=true eunit

$(PLTFILE):
	- dialyzer --build_plt --apps $(APP_DEPS) --output_plt $(PLTFILE)

dialyzer: compile $(PLTFILE)
	@dialyzer --fullpath --plt $(PLTFILE) -pa $(CURDIR)/ebin -c src --src | \
	fgrep -v -f ./dialyzer.ignore-warnings

typer:
	typer --plt $(PLTFILE) -I include -r src

shell:
	@$(ERL) $(ERLFLAGS)

clean-plt:
	@rm -rf $(PLTFILE)

clean-doc:
	@cd doc; ls * | grep -v overview.edoc | xargs rm -f

distclean: clean clean-plt clean-doc