
build:
		ocamlbuild -cflags -I,+extlib -j 0 devkit.cma devkit.cmxa

doc:
		ocamlbuild -cflags -I,+extlib -j 0 devkit.docdir/index.html

clean:
		ocamlbuild -clean
