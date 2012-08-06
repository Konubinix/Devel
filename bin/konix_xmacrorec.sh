#!/bin/bash

touch "$HOME/.konix_in_xmacro"
xmacrorec2 -k 67 > $HOME/.xmacro && konix_display.py "Macro added in $HOME/.xmacro"
rm "$HOME/.konix_in_xmacro"
