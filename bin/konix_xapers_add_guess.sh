#!/bin/bash

source "${KONIX_LIB_DIR}/konix_xapers_lib.sh"

set -eu
FILE="$1"
echo "######"
mimeopen "${FILE}" &
#set -x
echo "$FILE"
export KONIX_XAPERS_ADD_TAGS="`konix_gtk_entry.py -n -t "Tags" -i "${KONIX_XAPERS_ADD_TAGS}"`"
konix_xapers_gtk_guess_source
unset KONIX_XAPERS_ADD_AUTO_PROMPT
unset KONIX_XAPERS_ADD_AUTO_VIEW
konix_xapers_add.sh --file="${FILE}" "${SOURCE}"
echo "######"
