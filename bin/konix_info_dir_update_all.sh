#!/bin/bash

UPDATE_SCRIPT=`which konix_info_dir_update.sh`
for info_path in ${INFOPATH//:/ }
do
    echo "Updating $info_path"
    if konix_is_root_dir.sh "${info_path}"
    then
        echo "needs sudo priviledges"
        SUDO=sudo
    else
        SUDO=
    fi
    $SUDO "$UPDATE_SCRIPT" "${info_path}"
    echo "$info_path updated"
done
