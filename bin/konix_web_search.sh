#!/bin/bash

usage () {
    cat<<EOF
$0 [-h] [-e engine] [-d]
-d: default engine
-e: choose the engine
-h: show this
EOF
}

ENGINE=""
while getopts "he:d" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        e)
            ENGINE="$OPTARG"
            ;;
        d)
            ENGINE="`konix_web_search_engines.sh|head -1`"
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
URI=`konix_web_search_engine_find_uri.sh "$ENGINE"`
if [ -z "$1" ]
then
	RES=`zenity --entry --text "Search what in $ENGINE ?"`
    if [ -n "$RES" ]
    then
        set $RES
    else
        exit 1
    fi

fi
"$BROWSER" "${URI//%s/$*}"
