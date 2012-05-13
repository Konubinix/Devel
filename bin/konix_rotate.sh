#!/bin/bash -x

FILE_NAME="$1"
FILE_ROT="$1.rot.avi"

ffmpeg -i "$1" -r 30 -b:v 16M -vf "transpose=2" "$FILE_ROT"
