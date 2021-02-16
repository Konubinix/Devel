#!/bin/bash -eux

SUB=""
ARGS=""

usage () {
    cat<<EOF
$0 [-s][-h][-y]
y: auto yes
s: use subtitle files from a file with the same name as input in .webvtt format
    (hint: use submarine to convert the subtitle)
Description
EOF
}


while getopts "i:syh" opt; do
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
	# use submarine
	ARGS="${ARGS} -vf subtitles=${INPUT%*.*}.webvtt"
fi

avconv -i "${INPUT}" \
	   -c:v libvpx -c:a vorbis -ac 2 -f webm -b:v 750k -c:s webvtt -strict -2 \
	   ${ARGS} \
	   "${OUTPUT}"
