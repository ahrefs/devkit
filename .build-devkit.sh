#! /usr/bin/env bash
set -e -u

. /shared/ci-utils/opam-setup.sh

echo "-- install and upgrade opam deps"

cmd opam uninstall devkit
cmd opam upgrade --fixup
cmd opam install --with-doc --deps-only ./devkit.opam

echo "+++ build"

make BUILDFLAGS="-tag warn_error_A,warn_error_d" distclean lib test doc

if [ "$BUILDKITE" = "true" ]; then
  echo "--- build doc"
  mkdir _build/output/
  mv _build/default/_doc/_html _build/output/ocamldoc
  buildkite-agent meta-data set "DOC_PATH" "_build/output/"
fi
