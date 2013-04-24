#!/bin/bash

mkdir -p "$KONIX_PERSO_DIR/gnupg"
echo "Saving the public keys"
gpg --export > "$KONIX_PERSO_DIR/gnupg/pubring.gpg"
echo "Saving the ownertrust"
gpg --export-ownertrust > "$KONIX_PERSO_DIR/gnupg/trustdb.txt"
echo "Done"
