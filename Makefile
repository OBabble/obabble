FILES = $(basename $(wildcard *.ml))

all: $(addsuffix .byte, $(FILES))

%.byte : $(addsuffix .ml, $(FILES))
	ocamlbuild -use-ocamlfind $@

clean:
	rm -rf _build *.byte
