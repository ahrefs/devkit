
.PHONY: build top doc clean

build:
		ocamlbuild -j 0 devkit.cma devkit.cmxa

top:
		ocamlbuild -j 0 -cflags -thread -lflags -thread -libs unix,threads devkit.top

doc:
		ocamlbuild -j 0 devkit.docdir/index.html

clean:
		ocamlbuild -clean
