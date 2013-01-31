#!/bin/bash

USED_JOBS="${JOBS:-$(($(nproc) + 1))}"
make -j$USED_JOBS "$@"
