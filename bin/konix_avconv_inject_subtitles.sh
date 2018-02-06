#!/bin/bash -eux

MOVIE="$1"
SUBTITLES="$2"
FORMAT="${3:-webvtt}"

OUTPUT="${MOVIE%.*}_with_sub.${MOVIE#*.}"

avconv -i "${MOVIE}" -v:c copy -a:c copy \
	   -f "${FORMAT}" -i "${SUBTITLES}" -c:s "${FORMAT}" \
	   "${OUTPUT}"
