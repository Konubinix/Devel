#!/bin/bash
source _konix_gpgdir_variables.sh

if [ -e "$KONIX_GPGDIR_ENCRYPT_EXCLUDE" ]
then
    GPG_EXCLUDE_FROM="--Exclude-from $KONIX_GPGDIR_ENCRYPT_EXCLUDE"
fi
eval "gpgdir $* -a $GPG_EXCLUDE_FROM --overwrite-encrypted -e ."
