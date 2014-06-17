#!/bin/bash

set -eu
DATE="$(date +%s)"
DATE_READABLE="$(date -d @${DATE})"
LIST_PACKAGES="$(mktemp)"
trap "rm '${LIST_PACKAGES}'" 0

echo "Computing the list of packages"
aptitude search '~i ?not(~M) ?not(?user-tag(.))' > "${LIST_PACKAGES}"
NUMBER="$(wc -l "${LIST_PACKAGES}"|cut -f1 -d' ')"
LAST_RES="d"
while read -u 10 line
do
    NAME="$(echo "$line" | sed -r 's/^[^ ]+ +([^ ]+).+$/\1/')"
    echo "--------------- ${NUMBER} left"
	NUMBER=$((NUMBER-1))
    aptitude show "${NAME}"
    read -p "> Why is $NAME still here (${LAST_RES}) ? " RES
    if [ "${RES}" == "" ]
    then
        RES="${LAST_RES}"
    fi
    LAST_RES="${RES}"
    if [ "${RES}" == "d" ]
    then
        echo "Removing packages ${NAME}"
        sudo aptitude purge "${NAME}"
    else
        TAG="${DATE_READABLE} (${DATE}):${RES}"
        echo "Applying to ${NAME} the user tag '${TAG}'"
        sudo aptitude add-user-tag "${TAG}" "${NAME}"
    fi
done 10<"${LIST_PACKAGES}"
