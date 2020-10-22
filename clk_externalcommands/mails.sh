#!/bin/bash -x

list_accounts ( ) {
	grep '^accounts' $KONIX_OFFLINEIMAPRC |cut -f 2 -d'='|sed -r 's|([a-zA-Z]+)|"\1"|g'
}

usage () {
    cat<<EOF
$0

Fetch mails
--
O:-a,--account:[$(list_accounts)]:The account to sync
F:--mutex/--parallel:Ensure only one run at a time:True
N:Remaining args
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

echo "############"
date
echo "############"
LOG_FILE="$(mktemp)"

EXITVALUE=0

onerror () {
    EXITVALUE=1
}

onofflineimaperror () {
	konix_display.py -o "Errors when fetching mails"
    echo "Errors in offlineimap call" >&2
    cat "$LOG_FILE" >&2
    onerror
}

mutex ( ) {
	if [ "${CLK___MUTEX}" == "True" ]
	then
		konix_lock_run.sh -n -N offlineimap "$@"
	else
		"$@"
	fi
}

trap "echo > ${TMPDIR}/konix_mail_tray_stamp; rm '$LOG_FILE'" 0
{
	args=()
	if [ -n "${CLK___ACCOUNT}" ]
	then
		args+=("-a" "${CLK___ACCOUNT}")
	fi
	# timeout --kill-after=2m -s KILL "${KONIX_OFFLINEIMAP_TIMEOUT:-1200}"
    mutex offlineimap -c "${KONIX_OFFLINEIMAPRC}" "${args[@]}" "$@" 2>&1
    mutex konix_gmail_dump.sh 2>&1
} | tee "$LOG_FILE" || onofflineimaperror

mutex konix_sendmail_flush.sh

mutex konix_mail_new.sh "$@"

if [ "$(notmuch count tag:new and tag:google_invitation)" != "0" ]
then
    konix_google_invitation_notif.sh
fi

if grep -q ERROR "$LOG_FILE"
then
    onofflineimaperror
fi

# konix_mail_tray_daemon_update.py -d || onerror

konix_display.py -o "Mails synchronized"
exit $EXITVALUE
