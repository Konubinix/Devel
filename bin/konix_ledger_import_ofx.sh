#!/bin/bash

set -eu
OFX_FILE_PATH="$(readlink -f "${1}")"
OUTPUT_FILE="${OFX_FILE_PATH%%.ofx}.dat"
KONIX_LEDGER_BANK_DIR="$(dirname "${KONIX_LEDGER_BANK_FILE}")"
OUTPUT_FILE_NAME="$(basename "${OUTPUT_FILE}")"

konix_ofx_to_ledger.sh "${OFX_FILE_PATH}" >> "${OUTPUT_FILE}"
REL_PATH="$(konix_relative_path.py "${OUTPUT_FILE}" "${KONIX_LEDGER_BANK_DIR}")"
echo "include ${REL_PATH}" >> "${KONIX_LEDGER_BANK_FILE}"
