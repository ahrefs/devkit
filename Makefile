
.PHONY: all lib top doc clean

all:
		ocamlbuild -j 0 devkit.otarget

lib:
		ocamlbuild -j 0 devkit.cma devkit.cmxa

top:
		ocamlbuild -j 0 devkit.top

doc:
		ocamlbuild -j 0 devkit.docdir/index.html

clean:
		ocamlbuild -clean
