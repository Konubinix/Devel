#!/bin/bash

HISTORY_=`konix_dmenu_custom.sh < "${UZBL_HISTORY_FILE}"|cut -f3 -d' '`
if [ -n "$HISTORY_" ]
then
    "$BROWSER" "${HISTORY_}"
fi
