#!/bin/bash -x

xscreensaver-command -watch | while read line
do
    if echo "${line}" | grep "^LOCK "
    then
        rm -f "${HOME}/.here"
        konix_do_cron_job.sh konix_away_hook.sh
    fi
    if echo "${line}" | grep "^UNBLANK "
    then
        echo "1" > "${HOME}/.here"
        konix_do_cron_job.sh konix_back_hook.sh
    fi
done
