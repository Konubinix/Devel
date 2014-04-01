#!/bin/bash

USED_JOBS="${JOBS:-$(($(nproc) + 1))}"
"${KONIX_MAKE:-make}" -j$USED_JOBS "$@"
