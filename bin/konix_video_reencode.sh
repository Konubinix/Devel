#!/bin/bash

set -eu

video="$1"
video="$(konix_absolute_path.py "${video}")"
video_basename="$(basename "${video}")"
directory="$(dirname "${video}")"
original_directory="${directory}/original_videos"
original_video_new_location="${original_directory}/${video_basename}"
video_no_ext="${video%.*}"
new_file="${video_no_ext}.webm"
if [ -e "${new_file}" ]
then
    echo "I won't overwrite ${new_file}"
else
	# remove the file in case it is not complete
	trap "rm '${new_file}'" 0

	# By default the CRF value can be from 4–63, and 10 is a good starting point. Lower values mean better quality
	width=1024
	height=768
	downsize_force_ratio="-filter:v 'scale=iw*min($width/iw\,$height/ih):ih*min($width/iw\,$height/ih), pad=$width:$height:($width-iw*min($width/iw\,$height/ih))/2:($height-ih*min($width/iw\,$height/ih))/2'"
	downsize_keep_ratio="-vf scale='if(gt(a,$height/$width),$height,-1)':'if(gt(a,$height/$width),-1,$width)'"
	avconv -n \
		   -i "${video}" \
		   -codec:v vp8 \
           -r 30 \
		   -crf 20 \
		   -b:v 0 \
		   $downsize_keep_ratio \
		   -codec:a libvorbis \
		   "${new_file}"
	# no need to remove the file anymore
	trap "" 0
fi
mkdir -p "${original_directory}"
mv "${video}" "${original_video_new_location}"
