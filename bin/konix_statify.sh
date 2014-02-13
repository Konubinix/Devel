#!/bin/bash

EXEC_FILE="$1"
TMP_DIR="$(mktemp -d)"
trap "rm -rf '${TMP_DIR}'" 0
LIB_DIR="${TMP_DIR}/lib"
BIN_DIR="${TMP_DIR}/bin"
mkdir "${LIB_DIR}"
mkdir "${BIN_DIR}"
cp "${EXEC_FILE}" "${BIN_DIR}"
{
    # this computes the list of arguments to give to cp. It attempts to avoid
    # lsb stuffs.
    ldd "${EXEC_FILE}" \
        |grep '=>' \
        |sed 's/\t//' \
        |sed -r 's/ \(.+$//' \
        |sed -r 's/^.+ => //' \
        |grep -v 'libselinux\|libc.so\|libdl'
    echo "${LIB_DIR}"
} | xargs cp

cp "${KONIX_SHARE_DIR}"/statifier/* "${TMP_DIR}"
echo "$(basename ${EXEC_FILE})" > "${TMP_DIR}/.execname"

# clean the trap
trap "" 0
echo "Standalone made in ${TMP_DIR}"
