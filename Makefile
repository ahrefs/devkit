
.PHONY: build lib doc clean install uninstall test gen gen_ragel gen_metaocaml

OCAMLBUILD=ocamlbuild -use-ocamlfind -no-links -j 0

target: build

gen_ragel: devkit_ragel.ml
gen_metaocaml: extEnum_merge.ml

extEnum_merge.ml: stage_merge.ml
		OCAMLFIND_TOOLCHAIN=metaocaml ocamlfind ocamlc -linkpkg -package extlib stage_merge.ml -o stage_merge.byte
		rm stage_merge.cm*
		./stage_merge.byte > $@
		ocamlfind ocamlc -package extlib -i $@ > $@i

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
