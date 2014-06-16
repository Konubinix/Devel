#!/bin/bash

set -eu
pushd "${KONIX_SSHFS_ROOT}"
konix_sshfs_clean.sh
for machine in *
do
    read -p "Remove ${machine} ? "
    if [ "${REPLY}" == "y" ]
    then
        fusermount -u "${machine}"
        rmdir "${machine}"
        echo "${machine} removed"
    fi
done
