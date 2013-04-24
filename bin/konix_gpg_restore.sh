#!/bin/bash

echo "Restoring the public keys"
gpg --import < "$KONIX_PERSO_DIR/gnupg/pubring.gpg"
echo "Restoring the ownertrust"
gpg --import-ownertrust < "$KONIX_PERSO_DIR/gnupg/trustdb.txt"
echo "Done"
