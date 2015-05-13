#!/bin/bash

if ! [ -d "${KONIX_SMB_MOUNT}" ]
then
    mkdir "${KONIX_SMB_MOUNT}"
fi

smbnetfs "${KONIX_SMB_MOUNT}" "$@"
