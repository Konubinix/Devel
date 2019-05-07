#!/bin/bash
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

set -eu

clk-video.remove-15fps-videos.sh
clk-videos.rename.sh
clk-videos.reencode.sh
