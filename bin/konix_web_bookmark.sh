#!/bin/bash

BOOKMARK=`konix_dmenu_custom.sh < "${UZBL_BOOKMARKS_FILE}"|cut -f1 -d' '`
# if the scheme indicates so, the BOOKMARK must be evaled
if echo "$BOOKMARK" | grep -q -e "^eval://"
then
    BOOKMARK="$(echo "${BOOKMARK}"|sed 's-^eval://--'|sed 's/__/ /')"
    BOOKMARK="$(eval "echo "`echo "$BOOKMARK"`"")"
fi
if [ -n "$BOOKMARK" ]
then
    "$BROWSER" "$BOOKMARK"
fi
