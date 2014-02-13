#!/bin/bash

ME="$(readlink -f "${BASH_SOURCE[0]}")"
MY_DIR="$(dirname "${ME}")"
source "${MY_DIR}/init.sh" > /dev/null

exec "${EXEC_NAME}" "$@"
