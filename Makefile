FILES = $(basename $(wildcard *.ml))

all: $(addsuffix .byte, $(FILES))

tests: markov_tests.byte

%.byte: $(addsuffix .ml, $(FILES))
	ocamlbuild $@

clean:
	rm -rf _build *.byte

purge: clean
	rm  models/*
