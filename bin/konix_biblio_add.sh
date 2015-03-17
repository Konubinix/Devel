#!/bin/bash

source "${KONIX_LIB_DIR}/konix_xapers_lib.sh"

set -eu
tri="${KONIX_BIBLIO_DIR}/tri"
mkdir -p "${tri}"
cd "${tri}"
dled_file="$(git annex addurl "$@" | grep addurl | sed -r 's/^addurl (.+) \(downloading.+$/\1/')"
if [ "$?" != "0" ]
then
    exit 1
fi

if [ -z "${dled_file}" ]
then
    echo "Failed to process the url" >&2
    exit 1
fi
# see the file
mimeopen "${dled_file}" &

# adjust the environment
export KONIX_XAPERS_ADD_TAGS="`konix_gtk_entry.py -n -t "Tags" -i "${KONIX_XAPERS_ADD_TAGS}"`"
unset KONIX_XAPERS_ADD_AUTO_PROMPT
unset KONIX_XAPERS_ADD_AUTO_VIEW
export KONIX_XAPERS_ADD_AUTO_MOVE=1
konix_xapers_gtk_guess_source

konix_xapers_add.sh --file="${dled_file}" "${SOURCE}"
