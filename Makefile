
.PHONY: all lib top doc clean install uninstall test

INSTALL_FILES=$(wildcard _build/*.cmx _build/*.cmi _build/*.mli _build/devkit.cma _build/devkit.cmxa _build/*.lib _build/*.a)
OCAMLBUILD=ocamlbuild -j 0

all:
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

clean:
		ocamlbuild -clean

