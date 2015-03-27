#!/bin/bash

# launching the tool twice so that the orig files left by the rotating pass have
# the correct name

do_on_directory () {
    local DIR="${1}"
    echo "### DO ON ${DIR}"
    pushd "${DIR}"
    # renaming part
    for file in *.JPG *.jpg
    do
        echo "# handling '${file}'"
        konix_image_reorient.sh "${file}"
        konix_image_rename.sh "${file}"
    done
    popd
}

find -type d | while read dir
               do
                   do_on_directory "${dir}"
               done
