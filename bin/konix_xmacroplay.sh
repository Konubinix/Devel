#!/bin/bash

if [ -e "$HOME/.konix_in_xmacro" ]
then
    konix_display.py "Cannot execute the macro while it is being created"
else
    touch "$HOME/.konix_in_xmacro"
    xmacroplay :0 < $HOME/.xmacro
    rm "$HOME/.konix_in_xmacro"
fi
