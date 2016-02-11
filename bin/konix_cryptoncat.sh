#!/bin/bash

set -eux
ABS_PATH="$(realpath -e "$1")"
RELATIVE_PATH="$(realpath --relative-to="${KONIX_CRYPTON}" "${ABS_PATH}")"
konix_cryptonctl.sh cat "${RELATIVE_PATH}"
