#!/bin/bash -eux

SUB=""
ARGS=""
while getopts "i:sy" opt; do
	case $opt in
		h)
			usage
			exit 0
			;;
		i)
			INPUT="$OPTARG"
			;;
		s)
			SUB=1
			;;
		y)
			ARGS="${ARGS} -y"
			;;
	esac
done
shift $((OPTIND-1))
OUTPUT="${INPUT%*.*}.webm"
if [ -n "${SUB}" ]
then
	ARGS="${ARGS} -vf subtitles=${INPUT%*.*}.webvtt"
fi

avconv -i "${INPUT}" \
	   -c:v libvpx -c:a vorbis -ac 2 -f webm -b:v 2000k -c:s webvtt -strict -2 \
	   ${ARGS} \
	   "${OUTPUT}"
