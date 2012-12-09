#!/bin/bash

mkdir -p "$KONIX_PERSO_DIR/gnupg"
gpg --export > "$KONIX_PERSO_DIR/gnupg/pubring.gpg"
gpg --export-ownertrust > "$KONIX_PERSO_DIR/gnupg/trustdb.txt"
