#!/bin/bash -x

echo "############"
date
echo "############"
LOG_FILE="$(mktemp)"
konix_redis-cli.sh rpush mail_tray_daemon "?"

EXITVALUE=0

onerror () {
    konix_redis-cli.sh rpush mail_tray_daemon "p"
    EXITVALUE=1
}

onofflineimaperror () {
    echo "Errors in offlineimap call" >&2
    cat "$LOG_FILE" >&2
    onerror
}

trap "echo > ${TMPDIR}/konix_mail_tray_stamp; rm '$LOG_FILE'" 0
{
    konix_lock_run.sh -n -N offlineimap timeout --kill-after=2m -s KILL "${KONIX_OFFLINEIMAP_TIMEOUT:-1200}" offlineimap -c "${KONIX_OFFLINEIMAPRC}" "$@" 2>&1
    konix_lock_run.sh -n -N offlineimap gmail-dump.py 2>&1
} | tee "$LOG_FILE" || onofflineimaperror

konix_sendmail_flush.sh

konix_mail_new.sh "$@"

if [ "$(notmuch count tag:new and tag:google_invitation)" != "0" ]
then
    konix_google_invitation_notif.sh
fi

if grep -q ERROR "$LOG_FILE"
then
    onofflineimaperror
fi

konix_mail_tray_daemon_update.py -d || onerror

exit $EXITVALUE
