#!/bin/bash -eu

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

clk-images.convert.sh
clk-images.rename.sh
clk-images.rotate.sh
