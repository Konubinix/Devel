#!/bin/bash

if ! [ -d "${KONIX_SMB_MOUNT}" ]
then
    mkdir "${KONIX_SMB_MOUNT}"
fi

smbnetfs -obig_writes "${KONIX_SMB_MOUNT}" "$@"
