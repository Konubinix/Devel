#!/bin/bash

konix_xapers_gtk_guess_source () {
    set -eu
    SOURCE="$(konix_gtk_entry.py -t "Bibtex DWIM")"
    if echo "${SOURCE}" | grep -q '^@'
    then
        # a real bibtex content
        BIBTEX="$(mktemp)"
        trap "rm '${BIBTEX}'" 0
        echo "${SOURCE}" > "${BIBTEX}"
        SOURCE="${BIBTEX}"
    elif echo "${SOURCE}" | grep -q '^edit:'
    then
        BIBTEX="$(mktemp)"
        trap "rm '${BIBTEX}'" 0
        bibtex_fetcher.py "${SOURCE}" > "${BIBTEX}"
        SOURCE="${BIBTEX}"
    elif [ "${SOURCE}" == "" ]
    then
        SOURCE=""
    else
        SOURCE="$(bibtex_get_uri.py "${SOURCE}")"
    fi
    SOURCE="--source=${SOURCE}"
}
