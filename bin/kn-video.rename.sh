#!/bin/bash
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

set -eu

file="$1"
echo "# handling '${file}'"
#konix_media_extract_rename.sh "${file}"
kn media avconv-rename@sh "${file}"
