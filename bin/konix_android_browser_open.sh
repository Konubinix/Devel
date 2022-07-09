#! /bin/bash

FILE="$1"
#FILE="$(echo "$FILE"|iconv -f utf-8 -t iso-8859-1)"
test -n "$FILE" || { echo "Must provide an argument" ; exit 1 ; }

if test -e "${FILE}"
then
    FILE="$(realpath "${FILE}")"
    FILE="http://localhost:9645${FILE##${HOME}}"
fi

FILE="$(echo "${FILE}"|sed 's/%/%25/g'|sed 's/&/%26/g'|sed 's/=/%3D/g'|sed 's/@/%40/g'|sed 's/+/%2B/g')"
/system/bin/am start \
        --user 0 \
        -a android.intent.action.VIEW \
        -t text/html \
        -d "${FILE}"
