#!/bin/bash -x

am start -a android.intent.action.VIEW -t "$(mimetype -b "$1")" -d "file://$(readlink -f "$1")"
#-t text/html -d file:///sdcard/tpt/telepathy.freedesktop.org/doc/book/index.html
#-n org.zirco/.ui.activities.MainActivity
