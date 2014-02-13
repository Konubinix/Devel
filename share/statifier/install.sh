#!/bin/bash

MY_DIR="$(dirname "${BASH_SOURCE[0]}")"
source "${MY_DIR}/init.sh" > /dev/null

die ( ) {
    echo "ERROR: $*" >&2
    exit 1
}

warn ( ) {
    echo "WARNING: $*" >&2
}

INSTALL_DIR="${HOME}/bin"
echo "Install where ? (${INSTALL_DIR})"
read new_install_dir
if [ "${new_install_dir}" != "" ]
then
    INSTALL_DIR="${new_install_dir}"
fi
DEST="${INSTALL_DIR}/${EXEC_NAME}"

[ -d "${INSTALL_DIR}" ] || die "${INSTALL_DIR} is not a directory"
[ -f "${DEST}" ] && die "${DEST} already exists"
# check that the install dir is in the path
is_same_dir () {
    local dir1="$(readlink -f ${1})"
    local dir2="$(readlink -f ${2})"
    [ "${dir1}" == "${dir2}" ]
}
IFS=":"
found=""
for path_to_scan in ${PATH}
do
    if is_same_dir "${path_to_scan}" "${INSTALL_DIR}"
    then
        found="1"
        break
    fi
done
if [ "${found}" == "" ]
then
    warn "${INSTALL_DIR} not found in the PATH environment variable (installing anyway)"
fi

ln -s "${MY_DIR}/run.sh" "${DEST}"
