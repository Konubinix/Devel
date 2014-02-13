#!/bin/bash

touch "$HOME/.konix_in_xmacro"
# keycode 135 = Menu
xmacrorec2 -k 135 > $HOME/.xmacro && konix_display.py "Macro added in $HOME/.xmacro"
rm "$HOME/.konix_in_xmacro"
