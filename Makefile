
all: compile


compile: 
	find . -name '._*' -delete
	./rebar compile

clean:
	./rebar clean

get-deps:
	./rebar get-deps

start:
	run_erl -daemon /tmp/ /tmp/ ./test.erl


attach:
	/opt/erlyvideo/erts-5.8.4/bin/to_erl /tmp/