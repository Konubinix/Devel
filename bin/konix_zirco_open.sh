#! /bin/bash


am start -a android.intent.action.VIEW -t text/html -n org.zirco/.ui.activities.MainActivity -d file://$(readlink -f "$1")
