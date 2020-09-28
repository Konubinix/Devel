#!/bin/bash -eu

file="$1"
file_abs="$(konix_absolute_path.py "${file}")"
ext="${file##*.}"
dir="$(dirname "${file}")"
date_to_parse="$(konix_avconv_creation_date.sh "${file}")"
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
