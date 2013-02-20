#!/bin/bash

source "$HOME/init_bin/konix_init_lib.sh"
import_env
UNDO="${UZBL_UNDOLIST_FILE:-${XDG_DATA_HOME:-~/.local/share}/uzbl/undolist}"
UZBL="exec uzbl-browser"
if [ -e $UNDO ]; then
    URL=`tail -n 1 $UNDO`
    LINECOUNT=`cat $UNDO | wc -l`
    if [[ $LINECOUNT == 1 ]]; then
        $UZBL -u "$URL" &
        rm $UNDO
    else
        $UZBL -u "$URL" &
        sed -i '$d' $UNDO
    fi
fi
