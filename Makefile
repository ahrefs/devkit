
.PHONY: build lib top doc clean install uninstall test

INSTALL_FILES=$(filter-out \
  $(wildcard _build/myocamlbuild*), \
  $(wildcard _build/*.cmx _build/*.cmi _build/*.mli _build/*.cma _build/*.cmxa _build/pa_openin.cmo \
						 _build/*.lib _build/*.a _build/*.dll _build/*.so))
OCAMLBUILD=ocamlbuild -j 0

build:
		$(OCAMLBUILD) devkit.otarget

lib:
		$(OCAMLBUILD) devkit.cma devkit.cmxa

top:
		$(OCAMLBUILD) devkit.top

test:
		$(OCAMLBUILD) test.byte test.native
		./test.byte

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

