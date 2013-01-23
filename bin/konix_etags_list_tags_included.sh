#!/bin/bash

usage () {
    cat<<EOF
$0 [-h] <TAGS>
Print the list of TAGS files included by TAGS. It finds the tags files
recursively.

It assumes the TAGS_INCLUDES file has been used to generate tags
EOF
}
TAGS_FILE=""
while getopts "hi:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        i)
            TAGS_FILE="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
if [ "${TAGS_FILE}" == "" ]
then
    usage
    exit 0
fi

TAGS_INCLUDES="${TAGS_FILE}_INCLUDES"
if [ -r "${TAGS_INCLUDES}" ]
then
    INCLUDES=`cat "${TAGS_INCLUDES}"`
    IFS=$'\n'
    for file in ${INCLUDES}
    do
        echo "$file"
        "$0" -i "$file"
    done
fi
