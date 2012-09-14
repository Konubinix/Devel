#!/bin/bash

if [ "$1" ]
then
    echo "event REQ_NEW_WINDOW $1" > "$UZBL_FIFO"
else
    echo "event REQ_NEW_WINDOW $(xclip -o | sed s/\\\@/%40/g)" > "$UZBL_FIFO"
fi
