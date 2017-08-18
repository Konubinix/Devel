#!/bin/bash -eux

INPUT="${1}"
OUTPUT="${INPUT}.webm"
shift

avconv -i "${INPUT}" \
	   -c:v libvpx -c:a vorbis -ac 2 -f webm -b:v 2000k -c:s webvtt -strict -2 \
	   "${@}" \
	   "${OUTPUT}"
