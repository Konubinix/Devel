#!/bin/bash

TMP_FILE="$(mktemp)"
trap "rm -rf '$TMP_FILE'" 0
cat > "${TMP_FILE}"
konix_edit.sh "${TMP_FILE}" 2>&1 > /dev/null
cat "${TMP_FILE}"
