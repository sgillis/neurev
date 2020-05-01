.PHONY: cover

all: deps compile cover

compile:
	rebar3 compile

deps:
	rebar3 get-deps

cover:
	rebar3 ct --cover && rebar3 cover

shell:
	rebar3 as test compile && rebar3 as test shell --name "neurev@oria"
