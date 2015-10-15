#!/bin/bash

# Some mua like AppleMail like to add attachment inline. Notmuch doesn't
# consider them as attachment. This call will fix that.
konix_notmuch_mail_fix_attachment.py

echo "############"
date
echo "############"
LOG_FILE="$(mktemp)"
MAIL_TRAY_DAEMON_CTRL="/tmp/mail_tray_daemon_control"
echo "?" > "$MAIL_TRAY_DAEMON_CTRL"
echo "b" > "$MAIL_TRAY_DAEMON_CTRL"
trap "echo > /tmp/konix_mail_tray_stamp; echo B > '$MAIL_TRAY_DAEMON_CTRL' ; rm '$LOG_FILE'" 0

# make notmuch db consistent (earlier removed mail files etc)
notmuch new --verbose || exit 1

#sync with imap server
nice -n 10 timeout 1200  offlineimap -c "${KONIX_OFFLINEIMAPRC}" "$@" 2>&1 | tee "$LOG_FILE" || exit 1
#get messages from pop server
# getmail
#finally reflect externally changed maildir flags in notmuch tags
notmuch new || exit 1

#get messages from the spool, but after notmuch new because I don't want to be
#informed the cron jobs that did $0 ended...
# commented the things below because I don't like the cron messages that pollute
#my maildirs
# mkdir -p "${HOME}/Mail/mail.spool_${LOGNAME}"
# mb2md -s "/var/spool/mail/${LOGNAME}" -d "${HOME}/Mail/mail.spool_${LOGNAME}"

# this is useless since 0.5 -> notmuchsync -d -r --all --sync-deleted-tag
konix_mail_init_tags.sh || exit 1

# perform general tag manipulation
# mute mails new that are in a muted thread
MUTED_THREADS="$(notmuch search --output=threads tag:mute|cut -f 1 -d $' ')"
if [ "${MUTED_THREADS}" != "" ]
then
    notmuch tag -unread -inbox +muted -- tag:new and and not tag:sent ${MUTED_THREADS}
fi
# make new mail in loud thread automatically show up
LOUD_THREADS="$(notmuch search --output=threads tag:loud|cut -f 1 -d $' ')"
if [ "${LOUD_THREADS}" != "" ]
then
    notmuch tag +flagged +unread +inbox +louded -- tag:new and not tag:sent and ${LOUD_THREADS}
fi

konix_mail_tray_daemon_update.py -d || exit 1

konix_mail_unnew.sh || exit 1

if grep -q ERROR "$LOG_FILE"
then
    echo "Errors in offlineimap call" >&2
    cat "$LOG_FILE" >&2
    exit 1
fi
