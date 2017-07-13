#! /bin/sh
set -e -u

export OPAMKEEPBUILDDIR=true
export OPAMBUILDTEST=false
export OPAMYES=true

opam update
opam sw 4.04.1
eval $(opam config env)
opam uninstall devkit
opam upgrade --fixup
opam install --deps-only devkit

make BUILDFLAGS="-tag warn_error_A,warn_error_d" distclean lib test doc

if [ "$BUILDKITE" = "true" ]; then
  mkdir _build/output/
  mv _build/devkit.docdir _build/output/ocamldoc
  buildkite-agent meta-data set "DOC_PATH" "_build/output/"
fi
