#!/bin/bash

set -eu

file="$1"
file_abs="$(konix_absolute_path.py "${file}")"
ext="${file##*.}"
dir="$(dirname "${file}")"
creation_date="$(avconv -i "${file}" 2>&1 \
	| grep creation_time \
	| head -1 \
	| sed -r 's|^.+creation_time   : ([0-9][0-9][0-9][0-9]-[0-9]+-[0-9]+)[ T]([0-9]+:[0-9]+:[0-9]+).*$|\1T\2|')"
if avconv -i "${file}" 2>&1 |grep -q "handler_name.\+:.\+VideoHandle"
then
	# utc date
	date_to_parse="${creation_date}+0000"
else
	# local date
	date_to_parse="${creation_date}"
fi
if [ "${date_to_parse}" == "" ]
then
    echo "no new name" >&2
    exit 1
fi
new_file_basename="$(date -d"${date_to_parse}" '+%y%m%d_%H%M%S')"
new_file_name="${new_file_basename}.${ext}"
candidate="${dir}/${new_file_name}"
candidate_abs="$(konix_absolute_path.py "${candidate}")"
index=0
while [ -e "${candidate}" ] && [ "${candidate_abs}" != "${file_abs}" ]
do
	echo "${new_file_name} already exists, find another one" >&2
	new_file_name="${new_file_basename}_${index}.${ext}"
	candidate="${dir}/${new_file_name}"
	candidate_abs="$(konix_absolute_path.py "${candidate}")"
	index=$((index+1))
done
if [ "${candidate_abs}" == "${file_abs}" ]
then
	echo "${file} is already good" >&2
fi
echo "${candidate}"
