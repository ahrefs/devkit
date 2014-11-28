#! /bin/sh
set -e -u

export OPAMKEEPBUILDDIR=true
export OPAMBUILDTEST=true
export OPAMYES=true

opam update
opam sw 4.02.1
eval $(opam config env)
opam uninstall devkit
opam upgrade
opam install --deps-only devkit

make lib test
