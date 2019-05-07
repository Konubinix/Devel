#!/bin/bash

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

set -eu

file="$1"
NEW_NAME="`konix_avconv_new_name.sh "${file}"`"
if [ -z "${NEW_NAME}" ]
then
	echo "Could not find a new name fo ${file}"
	exit 1
fi

NEW_NAME_CP="$(konix_absolute_path.py "${NEW_NAME}")"
FILE_CP="$(konix_absolute_path.py "${file}")"
if [ "${NEW_NAME_CP}" != "${FILE_CP}" ]
then
    echo "'${file}' -> '${NEW_NAME}'"
    mv "${file}" "${NEW_NAME}"
else
    echo "'${file}' already has a stable name"
fi
