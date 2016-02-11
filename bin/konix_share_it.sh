#!/bin/bash

set -eu

SOURCE="${1}"
DEST="${2:-${KONIX_TMP_SHARE_DIR}}"
mkdir -p "${DEST}"
fsmount "file://${SOURCE}" "${DEST}"
trap "fusermount -u '${DEST}'" 0
konix_ftp_share.sh &
ftppid=$!
konix_http_share.sh &
httppid=$!
set +eu
set -x
read -p "Press enter to stop"
kill -9 $ftppid
kill -9 $httppid
wait $ftppid
wait $httppid
