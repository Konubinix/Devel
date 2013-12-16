#! /bin/bash


FILE="$1"
if test -e "${FILE}" && ! echo "$FILE"|grep -q -e '^/'
then
        FILE="$(pwd)/${FILE}"
fi
FILE="$(echo "${FILE}"|sed 's/%/%25/g'|sed 's/&/%26/g'|sed 's/=/%3D/g'|sed 's/@/%40/g'|sed 's/+/%2B/g')"
#FILE="$(readlink -f "$1")"
if test -n "$FILE"
then
FILE="$(echo ${FILE}|sed 's-^/home-/data/home-')"
FILE="file://$FILE"
else
FILE="$1"
fi
am start \
        -a android.intent.action.VIEW \
        -t text/html \
        -n org.zirco/.ui.activities.MainActivity \
        -d "${FILE}"
