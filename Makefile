REBAR = $(shell which rebar || echo ./rebar)
ENABLE_STATIC = no

all: deps-up egara

deps:
	rebar get-deps

deps-up: deps
	rebar update-deps

egara:
	ENABLE_STATIC=no rebar compile

run:
	erl -pa apps/*/ebin deps/*/ebin -s egara
