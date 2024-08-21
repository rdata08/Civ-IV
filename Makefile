.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

zip:
	rm -f civ.zip
	zip -r civ.zip . -x@exclude.lst