#!/bin/bash

set -eu

file="$1"
echo "# handling '${file}'"
#konix_media_extract_rename.sh "${file}"
konix_media_avconv_rename.sh "${file}"
