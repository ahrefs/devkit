#! /bin/sh
set -e -u

export OPAMKEEPBUILDDIR=true
export OPAMBUILDTEST=false
export OPAMYES=true

opam()
{
  echo opam "$@" 1>&2
  $(which opam) "$@"
}

opam update
opam sw set 4.05.0 || ( opam sw create 4.05.0; opam remote add -k git ahrefs git@git.ahrefs.com:ahrefs/opam)
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
