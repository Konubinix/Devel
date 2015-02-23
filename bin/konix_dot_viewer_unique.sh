#!/bin/bash -x

set -ue
FILE="$1"
LOCK="${FILE}.lock"
PID_FILE="${FILE}.pid"
if [ -e "${LOCK}" ]
then
    PID="$(cat "${PID_FILE}")"
    kill "${PID}"
fi
# wait for the lock file to be removed before doing anything
lockfile "${LOCK}"
dot -Tx11 "${FILE}" &
echo "$!" > "${PID_FILE}"
trap "rm -f '${PID_FILE}' '${LOCK}'" 0
wait %1
