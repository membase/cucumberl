SHELL=/bin/sh

EFLAGS=-pa ebin

.PHONY: ebins

all: ebins

ebins:
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make
	test -f `which rebar` && rebar escriptize

clean:
	rm -f tmp/*.cov.html erl_crash.dumpg
	rm -rf ebin
	cd examples && make clean

test:
	erl -pa ebin -noshell -s cucumberl test -s init stop
