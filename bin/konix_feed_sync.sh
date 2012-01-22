#!/bin/bash

if [ -z "$KONIX_FEED2IMAP_SKIP" ]
then
# do a rss -> imap work
	feed2imap -f "$KONIX_PERSO_DIR/feed2imaprc"
# clean the old rss
	feed2imap-cleaner -f "$KONIX_PERSO_DIR/feed2imaprc"
fi
