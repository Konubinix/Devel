#!/bin/bash -x

echo "############"
date
echo "############"
LOG_FILE="$(mktemp)"
MAIL_TRAY_DAEMON_CTRL="${TMPDIR}/mail_tray_daemon_control"
echo "?" > "$MAIL_TRAY_DAEMON_CTRL"
echo "b" > "$MAIL_TRAY_DAEMON_CTRL"
trap "echo > ${TMPDIR}/konix_mail_tray_stamp; echo B > '$MAIL_TRAY_DAEMON_CTRL' ; rm '$LOG_FILE'" 0

konix_lock_run.sh -n -N offlineimap timeout 1200  offlineimap -c "${KONIX_OFFLINEIMAPRC}" "$@" 2>&1 | tee "$LOG_FILE" || exit 1

konix_sendmail_flush.sh

konix_mail_new.sh "$@"

if grep -q ERROR "$LOG_FILE"
then
    echo "Errors in offlineimap call" >&2
    cat "$LOG_FILE" >&2
    exit 1
fi

konix_mail_tray_daemon_update.py -d || exit 1
