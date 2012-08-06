#!/bin/bash

XMACRO_FILE="$HOME/.konix_xmacro_quick_mode"
if [ -e "$XMACRO_FILE" ]
then
    konix_display.py "Toggling xmacro mode off"
    cat "$XMACRO_FILE" | xargs kill
    rm "$XMACRO_FILE"
else
    konix_display.py "Toggling xmacro mode on"
    xbindkeys -n -f "$KONIX_CONFIG_DIR/xbindkeys/xmacro" &
    echo $! > "$XMACRO_FILE"
fi
