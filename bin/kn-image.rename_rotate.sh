#!/bin/bash
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

file="$1"
echo "# handling '${file}'"
kn-image.reorient.sh "${file}"
kn-image.rename.sh "${file}"
