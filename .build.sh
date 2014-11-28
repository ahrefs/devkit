#! /bin/sh
set -e -u

opam update
opam sw 4.02.1
eval $(opam config env)
opam uninstall -y devkit
opam install -y --deps-only devkit
make lib test
