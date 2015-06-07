#!/bin/bash

file="$1"
echo "# handling '${file}'"
konix_image_reorient.sh "${file}"
konix_image_rename.sh "${file}"
