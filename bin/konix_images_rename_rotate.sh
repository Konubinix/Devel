#!/bin/bash


# launching the tool twice so that the orig files left by the rotating pass have
# the correct name

do_on_directory () {
    local DIR="${1}"
    echo "### DO ON ${DIR}"
    pushd "${DIR}"
    # renaming part
    find -name '*.JPG' -o -name '*.jpg' | parallel konix_image_rename_rotate.sh
    popd
}

find -type d | while read dir
               do
                   do_on_directory "${dir}"
               done
