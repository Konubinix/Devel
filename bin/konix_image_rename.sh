#!/bin/bash

set -eu
file="$1"
NEW_NAME="`konix_exif_new_name.py "${file}"`"
NEW_NAME_CP="$(konix_absolute_path.py "${NEW_NAME}")"
FILE_CP="$(konix_absolute_path.py "${file}")"
if [ "${NEW_NAME_CP}" != "${FILE_CP}" ]
then
    echo "'${file}' -> '${NEW_NAME}'"
    mv "${file}" "${NEW_NAME}"
else
    echo "'${file}' already has a stable name"
fi
