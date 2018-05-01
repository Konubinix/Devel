#!/bin/bash
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

set -eu

kn-video.remove-15fps-videos.sh
kn-videos.rename.sh
kn-videos.reencode.sh
