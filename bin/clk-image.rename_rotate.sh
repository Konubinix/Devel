#!/bin/bash
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

file="$1"
echo "# handling '${file}'"
clk-image.reorient.sh "${file}"
clk-image.rename.sh "${file}"
