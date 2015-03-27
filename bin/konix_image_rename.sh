#!/bin/bash

set -eu
file="$1"
NEW_NAME="`konix_exif_new_name.py ${file}`"
if [ "${NEW_NAME}" != "${file}" ]
then
    echo "${file} -> ${NEW_NAME}"
    mv "${file}" "${NEW_NAME}"
else
    echo "${file} already has a stable name"
fi
