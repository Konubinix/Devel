#!/bin/bash
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

find -\( -name '*webm' -o -name "*MOV" -\) -exec "clk-video.remove-15fps-video.sh" "{}" ";"
