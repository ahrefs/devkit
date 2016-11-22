#! /bin/sh
set -e -u

export OPAMKEEPBUILDDIR=true
export OPAMBUILDTEST=true
export OPAMYES=true

opam update
opam sw 4.02.3
eval $(opam config env)
opam uninstall devkit
opam upgrade --fixup
opam install --deps-only devkit

make BUILDFLAGS="-tag warn_error_A,warn_error_d" distclean lib test
