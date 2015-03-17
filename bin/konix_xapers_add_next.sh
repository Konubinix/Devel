#!/bin/bash

BIBLIO_SRC="${HOME}/.virtualenvs/biblio/bin/activate"
if [ -e "${BIBLIO_SRC}" ]
then
    source "${BIBLIO_SRC}"
fi

set -eu

CUR="$(find -maxdepth 1 -type l|sort|head -1)"
if [ -z "${CUR}" ]
then
    echo "No more file to check" >&2
    exit 1
fi

konix_xapers_add_guess.sh "${CUR}"
