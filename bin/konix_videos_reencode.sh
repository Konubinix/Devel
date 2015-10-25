#!/bin/bash

set -eu
find -iname '*.mov' | while read video
do
	HandBrakeCLI -2 -Z "Universal" -i "${video}" -o "${video}.mkv"
	rm -rf "${video}"
done
