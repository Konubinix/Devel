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
UNREAD_ELEMENTS="$(notmuch search tag:unread | wc -l)"
FLAGGED_ELEMENTS="$(notmuch search tag:flagged | wc -l)"
UNREAD_N_INBOX_ELEMENTS="$(notmuch search tag:unread and tag:inbox| wc -l)"
MAIL_TRAY_DAEMON_CTRL="/tmp/mail_tray_daemon_control"
if [ "$DO_DISPLAY" == "1" ] \
	&& [ "$INBOX_ELEMENTS" != "0" -o "$UNREAD_ELEMENTS" != "0" ]
then
	# new mails, display some message
	konix_display.py "Mail info :
$INBOX_ELEMENTS in the inbox
$UNREAD_ELEMENTS not read"
fi

if [ -p "$MAIL_TRAY_DAEMON_CTRL" ]
then
	if [ "$UNREAD_N_INBOX_ELEMENTS" != "0" ]
	then
		echo n > "$MAIL_TRAY_DAEMON_CTRL"
		echo V > "$MAIL_TRAY_DAEMON_CTRL"
	elif [ "$UNREAD_ELEMENTS" != "0" ]
	then
		echo u > "$MAIL_TRAY_DAEMON_CTRL"
		echo V > "$MAIL_TRAY_DAEMON_CTRL"
	elif [ "$FLAGGED_ELEMENTS" != "0" ]
	then
		echo f > "$MAIL_TRAY_DAEMON_CTRL"
		echo V > "$MAIL_TRAY_DAEMON_CTRL"
	else
		echo i > "$MAIL_TRAY_DAEMON_CTRL"
		echo v > "$MAIL_TRAY_DAEMON_CTRL"
	fi
fi
