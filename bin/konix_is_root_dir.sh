#!/bin/bash

USERID="$(ls -na "$1"|sed 's/ \+/ /g'|head -2|tail -1|cut -f3 -d$' ')"
if [ "$USERID" == "0" ]
then
    exit 0
else
    exit 1
fi
