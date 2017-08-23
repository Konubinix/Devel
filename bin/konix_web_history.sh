#!/bin/bash

HISTORY_FILE="${1:-${XDG_DATA_HOME}/qutebrowser/history.sqlite}"
HISTORY_=`sqlite3 "${HISTORY_FILE}" "select last_atime, url from CompletionHistory order by last_atime desc;"|konix_dmenu_custom.sh|cut -f2 -d'|'`
if [ -n "$HISTORY_" ]
then
    "$BROWSER" "${HISTORY_}"
fi
