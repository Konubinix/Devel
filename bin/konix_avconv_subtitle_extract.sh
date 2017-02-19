#!/bin/bash -eux

INPUT="${1}"
shift
avconv -i "${INPUT}" 2>&1 |grep Subtitle
echo "Use -map <stream_number> to choose another subtitle"
OUTPUT="${INPUT}.webvtt"
avconv -i "${INPUT}" -vn -an -f webvtt -loglevel error  "${@}" "${OUTPUT}"
