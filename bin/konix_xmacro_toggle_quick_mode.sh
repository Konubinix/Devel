#!/bin/bash

set -eu
XMACRO_FILE="$HOME/.konix_xmacro_quick_mode"

if [ -e "$XMACRO_FILE" ]
then
    konix_display.py "Toggling xmacro mode off"
    set +e
    cat "$XMACRO_FILE" | xargs kill
    set -e
    rm "$XMACRO_FILE"
else
    xbindkeys -n -f "$KONIX_CONFIG_DIR/xbindkeys/xmacro" &
    pid=$!
    if [ "$?" == "0" ]
    then
        konix_display.py "Toggling xmacro mode on, use F12 to rerun"
        echo "$pid" > "$XMACRO_FILE"
    else
        konix_display.py "Could not create the bind key"
    fi
fi
