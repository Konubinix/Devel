#!/bin/bash

set -ue
mkdir -p "${KONIX_CRYPTOFF}"
if which konix_dump_crypton-passphrase.sh > /dev/null
then
    encfs --extpass=konix_dump_crypton-passphrase.sh "${KONIX_CRYPTON}" "${KONIX_CRYPTOFF}"
else
    encfs "${KONIX_CRYPTON}" "${KONIX_CRYPTOFF}"
fi
