#!/bin/bash

set -eu

TMP="`mktemp`"
mv CMakeCache.txt "${TMP}"
trap "mv '${TMP}' ." 0

rm -rf *
