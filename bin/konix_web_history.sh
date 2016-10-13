#!/bin/bash

HISTORY_FILE="${1:-${XDG_DATA_HOME}/qutebrowser/history}"
HISTORY_=`tac "${HISTORY_FILE}"|konix_dmenu_custom.sh|cut -f2 -d' '`
if [ -n "$HISTORY_" ]
then
    "$BROWSER" "${HISTORY_}"
fi
