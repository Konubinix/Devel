#!/bin/bash
LOCK_FILE="/var/lock/$(basename "$0").lock"
if [ -f "$LOCK_FILE" ]
then
	echo "Already a $0 running"
	exit 1
else
	touch "$LOCK_FILE"
fi

# make notmuch db consistent (earlier removed mail files etc)
notmuch new --verbose
# sync maildir flags up with notmuch
notmuchsync -d -s --all --sync-deleted-tag

#sync with imap server
offlineimap -c "$KONIX_OFFLINEIMAPRC"
#get messages from pop server
# getmail

#finally reflect externally changed maildir flags in notmuch tags
notmuch new

#get messages from the spool, but after notmuch new because I don't want to be
#informed the cron jobs that did $0 ended...
# commented the things below because I don't like the cron messages that pollute
#my maildirs
# mkdir -p "${HOME}/Mail/mail.spool_${LOGNAME}"
# mb2md -s "/var/spool/mail/${LOGNAME}" -d "${HOME}/Mail/mail.spool_${LOGNAME}"

# this is useless since 0.5 -> notmuchsync -d -r --all --sync-deleted-tag
konix_mail_init_tags.sh

konix_mail_tray_daemon_update.sh -d

rm "$LOCK_FILE"
