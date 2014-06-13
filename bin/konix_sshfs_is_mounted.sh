#!/bin/bash

set -eu
MACHINE="$1"
DIR="${KONIX_SSHFS_ROOT}/${MACHINE}"
grep -q "${DIR} fuse.sshfs" /etc/mtab
