#!/bin/bash

DO_DISPLAY=0
while getopts ":d" opt; do
	case $opt in
		d)
			DO_DISPLAY=1
			;;
		\?)
			echo "Invalid option: -$OPTARG" >&2
			;;
	esac
done

INBOX_ELEMENTS="$(notmuch search tag:inbox | wc -l)"
UNREAD_N_INBOX_ELEMENTS="$(notmuch search tag:unread and tag:inbox| wc -l)"
UNREAD_ELEMENTS="$(notmuch search tag:unread | wc -l)"
FLAGGED_ELEMENTS="$(notmuch search tag:flagged | wc -l)"
FLAGGED_UNREAD_ELEMENTS="$(notmuch search tag:flagged and tag:unread | wc -l)"
MAIL_TRAY_DAEMON_CTRL="/tmp/mail_tray_daemon_control"
NEW_UNREAD_MAILS_COUNT="$(notmuch search tag:new and tag:unread | wc -l)"

if [ "$DO_DISPLAY" == "1" ] \
	&& [ "$NEW_UNREAD_MAILS_COUNT" -lt "10" ]
then
	# less than 10 new mails display all of them
    IFS=$'\n'
    for mail in $(notmuch search tag:flagged and tag:unread and tag:new)
    do
        # display only the summary part
        konix_display.py -d 10000 "${mail:23}"
    done
fi

if [ "$DO_DISPLAY" == "1" ] \
	&& [ "$INBOX_ELEMENTS" != "0" \
	-o "$UNREAD_ELEMENTS" != "0" \
	-o "$FLAGGED_ELEMENTS" != "0" ]
then
	# new mails, display some message
	konix_display.py -d 10000 "Mail info :
$INBOX_ELEMENTS in the inbox ($UNREAD_N_INBOX_ELEMENTS unread)
$UNREAD_ELEMENTS not read
$FLAGGED_ELEMENTS flagged (with $FLAGGED_UNREAD_ELEMENTS unread)
$NEW_UNREAD_MAILS_COUNT new unread mails."
fi

if [ -p "$MAIL_TRAY_DAEMON_CTRL" ]
then
	if [ "$UNREAD_N_INBOX_ELEMENTS" != "0" ]
	then
		echo N > "$MAIL_TRAY_DAEMON_CTRL"
	elif [ "$FLAGGED_UNREAD_ELEMENTS" != "0" ]
	then
		echo F > "$MAIL_TRAY_DAEMON_CTRL"
	elif [ "$FLAGGED_ELEMENTS" != "0" ]
	then
		echo f > "$MAIL_TRAY_DAEMON_CTRL"
	elif [ "$INBOX_ELEMENTS" != "0" ]
	then
		echo n > "$MAIL_TRAY_DAEMON_CTRL"
	elif [ "$UNREAD_ELEMENTS" != "0" ]
	then
		echo u > "$MAIL_TRAY_DAEMON_CTRL"
	else
		echo i > "$MAIL_TRAY_DAEMON_CTRL"
	fi
fi
