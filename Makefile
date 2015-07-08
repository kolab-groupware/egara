REBAR = $(shell which rebar 2>/dev/null || echo ./rebar)
ENABLE_STATIC = no

all: deps-up egara

deps:
	$(REBAR) get-deps

deps-up: deps
	$(REBAR) update-deps

egara:
	ENABLE_STATIC=no $(REBAR) compile

run:
	erl -pa apps/*/ebin deps/*/ebin -config app -s egara
