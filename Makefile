
.PHONY: build lib top doc clean install uninstall test gen

INSTALL_FILES=$(filter-out \
  _build/myocamlbuild% _build/test.cm%, \
  $(wildcard _build/*.cmx* _build/*.cmi _build/*.mli _build/*.cma _build/*.cmt* \
						 _build/*.lib _build/*.a _build/*.dll _build/*.so))
OCAMLBUILD=ocamlbuild -use-ocamlfind -no-links -j 0

target: build

gen: devkit_ragel.ml

%.ml: %.ml.rl
		ragel -O -F1 $< -o $@

build: lib top build-test

lib:
		$(OCAMLBUILD) $(BUILDFLAGS) devkit.cma devkit.cmxa

top:
		$(OCAMLBUILD) $(BUILDFLAGS) devkit.top

build-test:
		$(OCAMLBUILD) $(BUILDFLAGS) test.byte test.native

test: build-test
		_build/test.native

doc:
		$(OCAMLBUILD) devkit.docdir/index.html

install: lib
		ocamlfind install -patch-version "$(shell git describe --always)" devkit META $(INSTALL_FILES)

uninstall:
		ocamlfind remove devkit

reinstall:
		$(MAKE) uninstall
		$(MAKE) install

clean:
		ocamlbuild -clean

distclean: clean
