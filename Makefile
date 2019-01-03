
VERSION=$(shell git describe --always --long)

ifndef VERSION
VERSION=v0.7
endif

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

EXTRA_TARGETS := $(shell ocamlfind query gperftools -format "devkit_gperftools.cma devkit_gperftools.cmxa" 2> /dev/null)
EXTRA_TARGETS += $(shell ocamlfind query jemalloc_ctl -format "devkit_jemalloc.cma devkit_jemalloc.cmxa" 2> /dev/null)

lib:
		# FIXME EXTRA TARGETS
		dune build $(DUNEFLAGS)

top:
		dune utop $(DUNEFLAGS)

test:
		dune runtest $(DUNEFLAGS)

doc:
		$(OCAMLBUILD) devkit.docdir/index.html

install: lib
		ocamlfind install -patch-version "$(VERSION:v%=%)" devkit META $(sort $(INSTALL_FILES))

uninstall:
		ocamlfind remove devkit

reinstall:
		$(MAKE) uninstall
		$(MAKE) install

clean:
		ocamlbuild -clean

distclean: clean
