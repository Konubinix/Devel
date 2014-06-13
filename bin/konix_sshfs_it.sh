#!/bin/bash

set -eu
MACHINE="${1}"
REMOTE_PATH="${2}"
DIR="${KONIX_SSHFS_ROOT}/${MACHINE}"
mkdir -p "${DIR}"
sshfs "${MACHINE}:${REMOTE_PATH}" "${DIR}"
