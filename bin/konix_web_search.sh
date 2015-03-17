#!/bin/bash

usage () {
    cat<<EOF
$0 [-h] [-e engine] [-d]
-d: default engine
-e: choose the engine
-c: use clipboard content
-h: show this
EOF
}

ENGINE=""
CLIPBOARD=""
while getopts "he:dc" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        e)
            ENGINE="$OPTARG"
            ;;
        d)
            if [ -e "${KONIX_WEB_SEARCH_DEFAULT_ENGINE_FILE}" ]
            then
                ENGINE="$(cat "${KONIX_WEB_SEARCH_DEFAULT_ENGINE_FILE}")"
            else
                ENGINE="`konix_web_search_engines.sh|head -1`"
            fi
            ;;
        c)
            CLIPBOARD="1"
            ;;
    esac
done
shift $((OPTIND-1))

if [ -z "$ENGINE" ]
then
    ENGINE=`konix_web_search_select_engine.sh`
    if [ -z "$ENGINE" ]
    then
        exit 1
    fi
fi

if [ -n "${CLIPBOARD}" ]
then
    CONTENT="`xclip -o`"
else
    CONTENT="${1}"
fi

if [ -z "${CONTENT}" ]
then
	CONTENT=`konix_gtk_entry.py --text "Search what in $ENGINE ?"`
fi

if [ -n "$CONTENT" ]
then
    set ${CONTENT}
else
    exit 1
fi

URI=`konix_web_search_engine_find_uri.sh "$ENGINE"`
REQUEST="${*}"
REQUEST="${REQUEST// /+}"
URI="${URI//%s/${REQUEST}}"
# if the scheme indicates so, the URI must be evaled
if echo "$URI" | grep -q -e "^eval:"
then
    URI="${URI/eval:/}"
    URI="$(eval "`echo "$URI"`")"
fi
# percent encode the URI
URI="${URI//%20}"
konix_display.py "$BROWSER ${URI}"

"$BROWSER" "${URI}"
