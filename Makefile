
all:
	./rebar check-deps compile escriptize

deps:
	./rebar get-deps

clean:
	./rebar clean

dist-clean:
	rm -rf deps && ./rebar clean