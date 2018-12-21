#!/usr/bin/env bash

#
# Generate steps for devkit pipeline
#

set -e -u

{
    cat << EOF
steps:
  - label: "Build devkit"
    agents:
      linux: "true"
    command:
      - "./.build-devkit.sh"
EOF
} | buildkite-agent pipeline upload
