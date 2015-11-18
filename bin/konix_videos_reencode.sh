#!/bin/bash

set -eu
find \( -iname '*.mov' -o -iname "*.mp4" -o -iname "*.avi" \) \
 -exec "konix_video_reencode.sh" "{}" ";"
