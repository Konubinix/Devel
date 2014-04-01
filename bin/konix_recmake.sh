#!/bin/bash

set -eu

TMP="`mktemp`"
mv CMakeCache.txt "${TMP}"
trap "mv '${TMP}' CMakeCache.txt" 0

rm -rf *
cmake "$@"
