#!/bin/bash

set -eu
# i should not run this in parallel since one avconv process is already taking
# advantage of all my cpus
find \
	-name "original_videos" \
	-prune \
	-o \
	-name ".git" \
	-prune \
	-o \
	\( \
    -iname '*.mov' \
           -o -iname "*.mp4" \
           -o -iname "*.avi" \
           \) -exec konix_video_reencode.sh '{}' ';'
