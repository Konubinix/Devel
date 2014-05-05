#!/bin/bash

usage () {
    cat<<EOF
$0 -t tag_filename -f filename

EOF
}


set -eu
while getopts "ht:f:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        t)
            TAG_FILENAMES="$OPTARG"
            ;;
        f)
            FILENAME="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
IFS=":"
for tags_filename in ${TAG_FILENAMES}
do
    dir="$(dirname "${tags_filename}")"
    if konix_is_abs.py "${FILENAME}"
    then
        rel_file_path="$(konix_relative_path.py "${dir}" "${FILENAME}")"
    else
        rel_file_path="${FILENAME}"
    fi
    grep "^[^]*${rel_file_path}[^]*$" "${tags_filename}" \
        | sed -r 's/^(.+),.+$/\1/' | while read rel_path
    do
        echo "${dir}/${rel_path}"
    done
done
