
build:
		ocamlbuild -j 0 devkit.cma devkit.cmxa

doc:
		ocamlbuild -j 0 devkit.docdir/index.html

clean:
		ocamlbuild -clean
