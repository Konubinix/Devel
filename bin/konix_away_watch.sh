#!/bin/bash

xscreensaver-command -watch | while read line
do
    if echo "${line}" grep "^LOCK "
    then
        emacsclient -e "(run-hooks 'konix/away-hooks)"
    fi
    if echo "${line}" grep "^UNBLANK "
    then
        emacsclient -e "(run-hooks 'konix/back-hooks)"
    fi
done
