#!/bin/bash
#set -x
TAGS_FILES=TAGS

usage () {
    echo "$0 [-h] [-t TAGS_FILES] expressions..."
    echo "\t-h\t: display this usage help"
    echo "\t-t <TAGS_FILES>\t: comma separated tags files, default to 'TAGS'"
}

while getopts "ht:" opt; do
    case $opt in
        h)
            usage
            exit 2
            ;;
        t)
            TAGS_FILES="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
# ####################################################################################################
# Get refs
# ####################################################################################################
REF="$@"
if [ -z "$REF" ]
then
    echo "Nothing to search for"
    usage
    exit 3
fi

IFS=$',' # in order to separate files
sed -n -e "
# record the file name
/^$/ {
   n
   s/^\(.\+\),[0-9]\+$/\1/
   h
}
/^.\+$REF.\+$/ {
   /[]/ ! d
   G
   s/^\(.*\)\(.\+\)\(.\+\),\(.\+\)\n\(.\+\)$/\5:\3:\1 (\2, \4)/
   p
}
" $TAGS_FILES
