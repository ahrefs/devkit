
.PHONY: build lib doc clean install uninstall test gen gen_ragel gen_metaocaml archive

OCAMLBUILD=ocamlbuild -use-ocamlfind -no-links -j 0

target: build

gen_ragel: devkit_ragel.ml htmlStream_ragel.ml

gen_metaocaml:
		OCAMLFIND_TOOLCHAIN=metaocaml ocamlfind ocamlc -linkpkg -package extlib stage_merge.ml -o stage_merge.byte
		rm stage_merge.cm*
		./stage_merge.byte > extEnum_merge.ml
		ocamlfind ocamlc -package extlib -i extEnum_merge.ml > extEnum_merge.mli

%.ml: %.ml.rl
		ragel -O -F1 $< -o $@

build: lib

lib:
		dune build $(DUNEFLAGS)

top:
		dune utop $(DUNEFLAGS)

test:
		dune runtest $(DUNEFLAGS)

doc:
		dune build $(DUNEFLAGS) @doc

install: lib
		dune install

uninstall:
		dune uninstall

reinstall: uninstall install

clean:
		dune clean

distclean: clean

VERSION=$(shell git describe --tag --always)
NAME=devkit-$(VERSION)

archive:
	git archive --prefix=$(NAME)/ HEAD | bzip2 > $(NAME).tbz
