#!/bin/bash

set -eu
find \
	-name "original_videos" \
	-prune \
	-o \
	-name ".git" \
	-prune \
	-o \
	-\( \
    -iname '*.mov' \
           -o -iname "*.mp4" \
           -o -iname "*.avi" \
           -\) \
    -exec "konix_video_rename.sh" "{}" ";"
