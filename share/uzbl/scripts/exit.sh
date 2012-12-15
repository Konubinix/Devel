#!/bin/bash

UNDO="${UZBL_UNDOLIST_FILE:-${XDG_DATA_HOME:-~/.local/share}/uzbl/undolist}"
if [ -e $UNDO ]; then
    LINECOUNT=`cat $UNDO | wc -l`
    if [ $LINECOUNT -ge 100 ]; then
        sed -i "1d" $UNDO
    fi
fi
if [ "$UZBL_URI" != "" ]
then
    echo "$UZBL_URI" >> $UNDO
fi
