#!/usr/bin/env bash

#
# download ci-utils scripts
#

set -e -u

echo "--- setup ci-utils"

rsync -rlt --delete "/home/user/doc/tools/ci-utils" "."

echo "imported $(ls -1 ./ci-utils | wc -l) files from tools/ci-utils to ${PWD}/ci-utils"
