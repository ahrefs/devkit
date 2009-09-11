
build:
		ocamlbuild -cflags -I,+extlib -j 0 devkit.cma devkit.cmxa

doc:
		ocamlbuild devkit.docdir/index.html

clean:
		ocamlbuild -clean
