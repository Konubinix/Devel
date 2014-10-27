#!/bin/bash -x

FILE="${1}"
FILE="$(readlink -f "$FILE")"
MIME="$(mimetype -b $FILE)"
FILE_DROID="$(echo $FILE|sed 's-/home-/data/debian/home-')"
am start --user 0 -a android.intent.action.VIEW -t "${MIME}" -d "file://${FILE_DROID}"
#-t text/html -d file:///sdcard/tpt/telepathy.freedesktop.org/doc/book/index.html
#-n org.zirco/.ui.activities.MainActivity
