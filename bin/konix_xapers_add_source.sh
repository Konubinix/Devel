#!/bin/bash

source "${KONIX_LIB_DIR}/konix_xapers_lib.sh"

set -eu
if [ "$#" == "0" ]
then
    echo "Simply adding a source"
    set -- "--tags=${KONIX_XAPERS_ADD_TAGS}"
else
    konix_xapers_open_pdf.sh "${@}" &
fi

konix_xapers_gtk_guess_source
unset KONIX_XAPERS_ADD_AUTO_PROMPT
unset KONIX_XAPERS_ADD_AUTO_VIEW
xapers add "${SOURCE}" "${@}"
