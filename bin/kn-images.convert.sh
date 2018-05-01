#!/bin/bash -eu
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

find -name .git -prune -o -\( -name '*.CR2' -o -iname '*.pjm' -o -iname '*.tiff' -\) -print -exec "convert" "{}" "{}.jpg" ";"
