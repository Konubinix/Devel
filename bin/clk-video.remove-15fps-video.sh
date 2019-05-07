#!/bin/bash
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

set -eux

if [ "$(clk-video.fps.py "$1")" == "15" ]
then
	rm -v "$1"
fi
