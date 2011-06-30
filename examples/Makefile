SHELL=/bin/sh

EFLAGS=-pa ebin
ESCRIPT = test -f `which cucumberl` && `which cucumberl` || ../cucumberl

.PHONY: ebins

all: test

ebins:
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make

clean:
	rm -f tmp/*.cov.html erl_crash.dumpg
	rm -rf ebin

test: ebins
	../cucumberl
