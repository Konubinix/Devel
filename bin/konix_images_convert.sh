#!/bin/bash -eu

find -name .git -prune -o -\( -name '*.CR2' -\) -print -exec "convert" "{}" "{}.jpg" ";"
