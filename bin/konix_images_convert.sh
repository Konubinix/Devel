#!/bin/bash -eu

find -name .git -prune -o -\( -name '*.CR2' -o -iname '*.pjm' -o -iname '*.tiff' -\) -print -exec "convert" "{}" "{}.jpg" ";"
