#!/bin/bash

# use the one given as parameter and fallback to the uzbl history file
HISTORY_FILE="${1:-$UZBL_HISTORY_FILE}"
HISTORY_=`tac "${HISTORY_FILE}"|konix_dmenu_custom.sh|cut -f3 -d' '`
if [ -n "$HISTORY_" ]
then
    "$BROWSER" "${HISTORY_}"
fi
