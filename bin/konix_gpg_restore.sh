#!/bin/bash

gpg --import < "$KONIX_PERSO_DIR/gnupg/pubring.gpg"
gpg --import-ownertrust < "$KONIX_PERSO_DIR/gnupg/trustdb.txt"
