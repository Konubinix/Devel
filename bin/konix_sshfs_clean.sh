#!/bin/bash

set -eu
pushd "${KONIX_SSHFS_ROOT}"
for machine in *
do
    if ! konix_sshfs_is_mounted.sh "${machine}" \
        && read -p "Remove directory for ${machine}? " \
        && [ "${REPLY}" == "y" ]
    then
        rmdir "${machine}" \
            && echo "Removed ${machine}" \
            || echo "Could not remove ${machine}"
    fi
done
