#!/bin/bash

set -eu
tri="${KONIX_BIBLIO_DIR}/tri"
mkdir -p "${tri}"
cd "${tri}"
git annex addurl --fast "$@"
