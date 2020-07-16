#!/bin/bash

set -eu
OFX_FILE_PATH="$(readlink -f "${1}")"
OFX_FILE_NAME="$(basename "${OFX_FILE_PATH}")"
FID=1
DATE="$(date)"
ACCOUNT="Assets:Bank:$(konix_ofx_get_info.sh -l "${OFX_FILE_NAME}")"

# ledger does not like brackets
sed -i 's/[()]/_/g' "${OFX_FILE_PATH}"

# ledger-autosync does not explicitly encode output
export PYTHONIOENCODING=utf_8

CMD="ledger-autosync --unknown-account wtf --assertions --fid 1 -a ${ACCOUNT}"
if ! konix_ledger.sh accounts|grep -q "${ACCOUNT}"
then
    CMD="${CMD} --initial"
fi
CMD="$CMD ${OFX_FILE_PATH}"
{
    echo ""
    echo "; ${DATE}"
    echo "; import command: $CMD"
    echo ""
    ${CMD}
    echo "; end of import"
}
