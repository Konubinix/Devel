#!/bin/bash
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

set -eu
find \
	-name "original_videos" \
	-prune \
	-o \
	-name ".git" \
	-prune \
	-o \
	-\( \
    -iname '*.mov' \
           -o -iname "*.mp4" \
           -o -iname "*.avi" \
           -o -iname "*.3gp" \
           -o -iname "*.mkv" \
           -\) \
    -exec "clk-video.rename.sh" "{}" ";"
