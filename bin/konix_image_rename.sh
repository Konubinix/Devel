#!/bin/bash

set -eu
file="$1"
NEW_NAME="`konix_exif_new_name.py ${file}`"
NEW_NAME_CP="$(readlink -f "${NEW_NAME}")"
FILE_CP="$(readlink -f "${file}")"
if [ "${NEW_NAME_CP}" != "${FILE_CP}" ]
then
    echo "${file} -> ${NEW_NAME}"
    mv "${file}" "${NEW_NAME}"
else
    echo "${file} already has a stable name"
fi
