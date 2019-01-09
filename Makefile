
.PHONY: build lib doc clean install uninstall test gen gen_ragel gen_metaocaml

INSTALL_FILES=$(filter-out \
  _build/myocamlbuild% _build/test.cm% _build/extEnum_merge.cmi _build/stage_merge.%, \
  $(wildcard _build/*.cmx* _build/*.cmi _build/*.mli _build/*.ml _build/*.cma _build/*.cmt* \
						 _build/*.lib _build/*.a _build/*.dll _build/*.so))

OCAMLBUILD=ocamlbuild -use-ocamlfind -no-links -j 0

target: build

gen_ragel: devkit_ragel.ml
gen_metaocaml: extEnum_merge.ml

extEnum_merge.ml: stage_merge.ml
		OCAMLFIND_TOOLCHAIN=metaocaml $(OCAMLBUILD) stage_merge.byte
		./_build/stage_merge.byte > $@
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
