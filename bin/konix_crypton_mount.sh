#!/bin/bash

set -ue
CRYPTON="${KONIX_PERSO_DIR}/crypton"
CRYPTOFF="${HOME}/cryptoff"
mkdir -p "${CRYPTOFF}"
if which konix_dump_crypton-passphrase.sh > /dev/null
then
    encfs --extpass=konix_dump_crypton-passphrase.sh "${CRYPTON}" "${CRYPTOFF}"
else
    encfs "${CRYPTON}" "${CRYPTOFF}"
fi
