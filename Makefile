
.PHONY: all lib top doc clean

all:
		ocamlbuild -j 0 devkit.otarget

lib:
		ocamlbuild -j 0 devkit.cma devkit.cmxa

top:
		ocamlbuild -j 0 devkit.top

doc:
		ocamlbuild -j 0 devkit.docdir/index.html

install: all
		ocamlfind install devkit META _build/*.cmi _build/devkit.cma _build/devkit.cmxa

remove:
		ocamlfind remove devkit

clean:
		ocamlbuild -clean
