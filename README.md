devkit
======

[![Build Status](https://travis-ci.org/ahrefs/devkit.svg?branch=master)](https://travis-ci.org/ahrefs/devkit)

General purpose OCaml library (development kit)
Copyright (c) 2009 Ahrefs
Released under the terms of LGPL-2.1 with OCaml linking exception.

`opam install devkit`

Development
===========

Install OCaml dependencies :

  opam install . --deps-only

External dependencies :

  opam list -s -e --resolve=devkit

To update ragel-generated code :

  aptitude install ragel
  make -B gen_ragel

To update metaocaml-generated code :

  opam exec --switch=4.07.1+BER -- make gen_metaocaml
