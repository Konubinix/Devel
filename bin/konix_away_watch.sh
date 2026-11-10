#!/usr/bin/env bash
set -x

xscreensaver-command -watch | while read line
do
    if echo "${line}" | grep "^LOCK "
    then
        rm -f "${HOME}/.here"
        konix_away_hook.sh
    fi
    if echo "${line}" | grep "^UNBLANK "
    then
        echo "1" > "${HOME}/.here"
        konix_back_hook.sh
    fi
done
