#!/bin/bash -eux

INPUT="$1"
SIZE="${2:-300}"
avconv -i "$INPUT" -f segment -segment_time "$SIZE" -reset_timestamps 1 -c copy %04d.ogg
