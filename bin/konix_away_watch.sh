#!/bin/bash

xscreensaver-command -watch | while read line
do
    if echo "${line}" grep "^LOCK "
    then
        rm -f "${HOME}/.here"
        emacsclient -e "(run-hooks 'konix/away-hooks)"
    fi
    if echo "${line}" grep "^UNBLANK "
    then
        echo "1" > "${HOME}/.here"
        emacsclient -e "(run-hooks 'konix/back-hooks)"
    fi
done
