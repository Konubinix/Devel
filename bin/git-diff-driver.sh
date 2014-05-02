#!/bin/bash

set -eu
path="$1"
old_file="$2"
old_hex="$3"
old_mode="$4"
new_file="$5"
new_hex="$6"
new_mode="$7"

if [[ "${old_file}" =~ ".dia" ]] &&  [[ "${new_file}" =~ ".dia" ]]
then
    old_file2=`mktemp`
    new_file2=`mktemp`
    zcat "${old_file}" > "${old_file2}"
    zcat "${new_file}" > "${new_file2}"
    trap "rm '${old_file2}' '${new_file2}'" 0
    old_file="${old_file2}"
    new_file="${new_file2}"
fi
echo $*
diff -u "${old_file}" "${new_file}"
