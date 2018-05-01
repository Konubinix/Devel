#!/bin/bash -eu

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

kn-images.convert.sh
kn-images.rename.sh
kn-images.rotate.sh
