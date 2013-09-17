
.PHONY: build lib top doc clean install uninstall test gen

INSTALL_FILES=$(filter-out \
  _build/myocamlbuild% _build/test.cm% _build/version.cm%, \
  $(wildcard _build/*.cmx _build/*.cmi _build/*.mli _build/*.cma _build/*.cmxa \
						 _build/*.lib _build/*.a _build/*.dll _build/*.so))
OCAMLBUILD=ocamlbuild -use-ocamlfind -no-links -j 0

target: build

gen: devkit_ragel.ml

%.ml: %.ml.rl
		ragel -O -F1 $< -o $@

build:
		$(OCAMLBUILD) devkit.otarget

lib:
		$(OCAMLBUILD) devkit.cma devkit.cmxa

top:
		$(OCAMLBUILD) devkit.top

test:
		$(OCAMLBUILD) test.byte test.native
		_build/test.byte

doc:
		$(OCAMLBUILD) devkit.docdir/index.html

install: lib
		ocamlfind install -patch-version "svn $(shell svnversion)" devkit META $(INSTALL_FILES)

uninstall:
		ocamlfind remove devkit

reinstall:
		$(MAKE) uninstall
		$(MAKE) install

clean:
		ocamlbuild -clean

