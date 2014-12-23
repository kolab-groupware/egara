REBAR = $(shell which rebar || echo ./rebar)
ENABLE_STATIC = no

all: app

deps:
	rebar get-deps

app: deps
	ENABLE_STATIC=no rebar compile

run:
	erl -pa apps/*/ebin deps/*/ebin -s bonnie
