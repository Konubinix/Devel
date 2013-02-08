#!/bin/bash

BOOKMARK=`konix_dmenu_custom.sh < "${UZBL_BOOKMARKS_FILE}"|cut -f1 -d' '`
if [ -n "$BOOKMARK" ]
then
    "$BROWSER" "$BOOKMARK"
fi
