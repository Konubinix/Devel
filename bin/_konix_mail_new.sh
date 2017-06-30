#!/bin/bash

notmuch new || exit 1
# Some mua like AppleMail like to add attachment inline. Notmuch doesn't
# consider them as attachment. This call will fix that.
konix_notmuch_mail_fix_attachment.py
#finally reflect externally changed maildir flags in notmuch tags
notmuch new || exit 1

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
