REBAR = $(shell which rebar3 2>/dev/null || echo ./rebar3)
ENABLE_STATIC = no

all: ENABLE_STATIC=$(ENABLE_STATIC) $(REBAR) compile

run:
	$(REBAR) shell --config app.config --apps egara

release:
	$(REBAR) release

tests:
	$(REBAR) eunit

