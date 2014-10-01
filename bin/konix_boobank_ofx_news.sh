#!/bin/bash

set -eu
test -e "${LEDGER_FILE}" || {
    echo "Ledger file not found in ${LEDGER_FILE}"
    exit 1
}
for i in ofx_*
do
	echo "Importing in $i"
	pushd "$i"
	konix_boobank_ofx_new.sh
	popd
done
