#!/bin/bash

set -eu

file="$1"
ext="${file##*.}"
dir="$(dirname "${file}")"
new_file_basename="$(avconv -i "${file}" 2>&1 \
	| grep creation_time \
	| head -1 \
	| sed -r 's|^.+creation_time   : [0-9][0-9]([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+):([0-9]+).*$|\1\2\3_\4\5\6|')"

new_file_name="${new_file_basename}.${ext}"
candidate="${dir}/${new_file_name}"
index=0
while [ -e "${candidate}" ]
do
	echo "${new_file_name} already exists, find another one" >&2
	new_file_name="${new_file_basename}_${index}.${ext}"
	candidate="${dir}/${new_file_name}"
	index=$((index+1))
done
echo "${candidate}"
