#!/bin/bash
# set -x
TAGS_FILES=TAGS

usage () {
    echo "$0 [-h] [-r] [-i] [-t TAGS_FILES] expressions..."
    echo "\t-h\t: display this usage help"
    echo "\t-r\t: recursive search"
    echo "\t-i\t: ignore case"
    echo "\t-t <TAGS_FILES>\t: comma separated tags files, default to 'TAGS'"
}

RECURSIVE=""
CASE=""
while getopts "hrit:" opt; do
    case $opt in
        h)
            usage
            exit 2
            ;;
        t)
            TAGS_FILES="$OPTARG"
            ;;
        r)
            RECURSIVE="1"
            ;;
        i)
            CASE="I"
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
ALL_TAGS_FILES="${TAGS_FILES//,/
}"

# recursively find all the tags files
if [ -n "$RECURSIVE" ]
then
    IFS=$',' # in order to separate files
    for tag in $TAGS_FILES
    do
        new_tags=`konix_etags_list_tags_included.sh -i "$tag"`
        if [ "$?" != "0" ]
        then
            echo "konix_etags_list_tags_included.sh -i $tag failed"
            exit 1
        fi
        ALL_TAGS_FILES="${ALL_TAGS_FILES}
$new_tags"
    done
fi

IFS=$'\n'
PWD=`pwd`
for tag_file in $ALL_TAGS_FILES
do
    # adjust the path relatively to pwd
    tag_dir=`dirname "$tag_file"`
    relpath=`konix_relative_path.sh "$PWD" "$tag_dir"`
    if [ -n "$relpath" ]
    then
        relpath="$relpath/"
    fi

    sed -n -e "
# record the file name
/^$/ {
   n
   s/^\(.\+\),[0-9]\+$/\1/
   h
}
/^.\+$REF.\+$/$CASE {
   /[]/ ! d
   G
   s|^\(.*\)\(.\+\)\(.\+\),\(.\+\)\n\(.\+\)$|$relpath\5:\3:\1 (\2, \4)|
   p
}
" "$tag_file"
done
