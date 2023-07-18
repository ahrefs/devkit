devkit
======

[![Build Status](https://github.com/ahrefs/devkit/actions/workflows/makefile.yml/badge.svg)](https://github.com/ahrefs/devkit/actions/workflows/makefile.yml)

General purpose OCaml library (development kit)
Copyright (c) 2009 Ahrefs
Released under the terms of LGPL-2.1 with OCaml linking exception.

    opam install devkit

Development
-----------

Install OCaml dependencies in your current / global switch:

    opam install . --deps-only

Or to install them in a new, directory-local switch:

    opam switch create . --deps-only --no-install
    opam install . --deps-only --with-test

External dependencies:

    opam list -s -e --resolve=devkit

To update ragel-generated code:

    aptitude install ragel
    make -B gen_ragel

To update metaocaml-generated code:

    opam exec --switch=4.07.1+BER -- make gen_metaocaml
