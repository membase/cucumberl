SHELL=/bin/sh

EFLAGS=-pa ebin

.PHONY: ebins

all: ebins

ebins:
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make

clean:
	rm -f tmp/*.cov.html erl_crash.dumpg
	rm -rf ebin

test:
	erl -pa ebin -noshell -s cucumberl test -s init stop

sample:
	erl -pa ebin -noshell -s sample main -s init stop

sample_more:
	erl -pa ebin -noshell -s sample_more main -s init stop
