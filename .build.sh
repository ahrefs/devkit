#! /bin/sh
set -e -u

opam sw 4.02.1
opam uninstall -y devkit
opam install -y --deps-only devkit
make lib test
