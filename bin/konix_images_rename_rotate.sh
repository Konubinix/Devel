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
        NEW_NAME="`konix_exif_new_name.py ${file}`"
        if [ "${NEW_NAME}" != "${file}" ]
        then
            echo "${file} -> ${NEW_NAME}"
            mv "${file}" "${NEW_NAME}"
        else
            echo "${file} already has a stable name"
        fi
    done
    popd
}

find -type d | while read dir
               do
                   do_on_directory "${dir}"
               done
