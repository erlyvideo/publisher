
all: compile


compile: 
	find . -name '._*' -delete
	./rebar compile

clean:
	./rebar clean

get-deps:
	./rebar get-deps

