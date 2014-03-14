#!/bin/bash

set -eu
echo "Restoring the public keys"
cat "${KONIX_PERSO_DIR}/gnupg_nd/pubring.gpg" \
    | gpg --armor --decrypt \
    | gpg --import
echo "Restoring the ownertrust"
cat "${KONIX_PERSO_DIR}/gnupg_nd/trustdb.txt.gpg" \
    | gpg --armor --decrypt \
    | gpg --import-ownertrust
echo "Done"
