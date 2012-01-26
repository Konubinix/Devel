#!/bin/bash

# do a rss -> imap work
feed2imap -v -f "$KONIX_PERSO_DIR/feed2imaprc"
# clean the old rss
feed2imap-cleaner -v -f "$KONIX_PERSO_DIR/feed2imaprc"
