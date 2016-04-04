#!/bin/bash -xeu

set -eu
video="$1"
video_no_ext="${video%.*}"
new_file="${video_no_ext}.mkv"
if [ -e "${new_file}" ]
then
    echo "I won't overwrite ${new_file}"
    exit 1
fi

#HandBrakeCLI -2 -Z "Universal" -i "${video}" -o "${video_no_ext}.mkv"
avconv -n \
	   -i "${video}" \
	   -r 30 \
	   -f mp4 \
	   -vf "scale=-1:576" \
	   -vcodec libx264 \
	   -vprofile high \
	   -preset slower \
	   -b:v 2000k \
	   -maxrate 2000k \
	   -acodec libmp3lame \
	   -map_metadata 0:s:0 \
	   -ab 196k \
	   "${video_no_ext}.mkv"
#rm -rf "${video}"
