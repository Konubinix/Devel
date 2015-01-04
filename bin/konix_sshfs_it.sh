#!/bin/bash

set -eu
MACHINE="${1}"
set +u
REMOTE_PATH="${2}"
set -u
DIR="${KONIX_SSHFS_ROOT}/${MACHINE}"
mkdir -p "${DIR}"
sshfs -ofollow_symlinks "${MACHINE}:${REMOTE_PATH}" "${DIR}"
