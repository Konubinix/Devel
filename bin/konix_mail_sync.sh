#!/bin/bash

# make notmuch db consistent (earlier removed mail files etc)
notmuch new
# sync maildir flags up with notmuch
notmuchsync -d -s --all --sync-deleted-tag

#sync with imap server
offlineimap
#get messages from pop server
# getmail

#finally reflect externally changed maildir flags in notmuch tags
NOTMUCH_RESP="$(notmuch new)"
echo "$NOTMUCH_RESP"
if ! echo "$NOTMUCH_RESP" | grep -q -r "^No new mail.$"
then
	# new mails, display some message
	konix_display.py "$NOTMUCH_RESP"
fi

#get messages from the spool, but after notmuch new because I don't want to be
#informed the cron jobs that did $0 ended...
mkdir -p "${HOME}/Mail/mail.spool_${LOGNAME}"
mb2md -s "/var/spool/mail/${LOGNAME}" -d "${HOME}/Mail/mail.spool_${LOGNAME}"

# this is useless since 0.5 -> notmuchsync -d -r --all --sync-deleted-tag
konix_mail_init_tags.sh
