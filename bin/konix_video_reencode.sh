#!/bin/bash -xeu

video="$1"
video_no_ext="${video%.*}"
#HandBrakeCLI -2 -Z "Universal" -i "${video}" -o "${video_no_ext}.mkv"
avconv -i "${video}" \
	   -r 30 \
	   -f mp4 \
	   -s 720x576  \
	   -vcodec libx264 \
	   -vprofile high \
	   -preset slower \
	   -b:v 2000k \
	   -maxrate 2000k \
	   -acodec libvo_aacenc \
	   -ab 196k \
	   "${video_no_ext}.mkv"
rm -rf "${video}"
