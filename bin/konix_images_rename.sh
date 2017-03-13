#!/bin/bash -eu

find -name .git -prune -o -\( -name '*.JPG' -o -name '*.jpg' -\) -exec "konix_image_rename.sh" "{}" ";"
