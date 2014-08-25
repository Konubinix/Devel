#!/bin/bash

set -eu
source "${KONIX_LIB_DIR}/remote_config_lib.sh"

usage () {
    cat<<EOF
$0 [-h] [-a] name

-h: print this and exit
-a: show the url for all contexts
name: name of the machine of which to print the context
EOF
}

ALL=""
while getopts "ha" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        a)
            ALL="1"
            ;;
    esac
done
shift $((OPTIND-1))

HOST="${1}"
remoterc_quiet
if [ -n "${ALL}" ]
then
    remoterc_compute_contexts
    for context_path in "${KONIX_REMOTE_CONTEXTS_DIR}/${HOST}"/*
    do
        context="$(basename ${context_path})"
        if echo "${contexts}" | grep -q "\b${context}\b"
        then
            echo -n "A:"
        else
            echo -n "U:"
        fi
        echo "${context}: $(cat "${context_path}/url")"
    done
else
    remoterc_setup_or_quit "${HOST}"
    echo -n "${remote_url}"
fi
