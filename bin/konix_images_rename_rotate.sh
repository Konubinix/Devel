#!/bin/bash

# launching the tool twice so that the orig files left by the rotating pass have
# the correct name

do_on_directory () {
    local DIR="${1}"
    echo "### DO ON ${DIR}"
    pushd "${DIR}"
    # renaming part
    renrot -n "%y%m%d_%H%M%S_#Model#" --no-rotate *.JPG *.jpg
    # rotating part
    renrot --no-rename *.JPG *.jpg
    popd
}

find -type d | while read dir
do
    do_on_directory "${dir}"
done
