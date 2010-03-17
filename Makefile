
.PHONY: all lib top doc clean install uninstall

INSTALL_FILES=$(wildcard _build/*.cmx _build/*.cmi _build/*.mli _build/devkit.cma _build/devkit.cmxa _build/*.lib _build/*.a)

all:
		ocamlbuild -j 0 devkit.otarget

lib:
		ocamlbuild -j 0 devkit.cma devkit.cmxa

top:
		ocamlbuild -j 0 devkit.top

doc:
		ocamlbuild -j 0 devkit.docdir/index.html

install: lib
		ocamlfind install -patch-version "svn $(shell svnversion)" devkit META $(INSTALL_FILES)

uninstall:
		ocamlfind remove devkit

clean:
		ocamlbuild -clean
