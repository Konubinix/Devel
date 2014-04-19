#!/bin/bash

set -eu

usage () {
    cat<<EOF
$0 -p PORT -h HOST [-w WAITING_TIME]

Starts a local proxy on port PORT tunnelling traffic through ssh connection with
HOST

It checks every WAITING_TIME seconds (defaults to 300 = 5 minutes) and attemps
to restart the connection if it appears broken.
EOF
}

WAITING_TIME="300"
while getopts "w:p:H:h" opt; do
	case $opt in
		h)
			usage
			exit 0
			;;
		p)
			PORT="$OPTARG"
			;;
		H)
			HOST="$OPTARG"
			;;
		w)
			WAITING_TIME="$OPTARG"
			;;
	esac
done
shift $((OPTIND-1))

echo "HOST=$HOST, PORT=$PORT, WAITING_TIME=$WAITING_TIME"
while true
do
	echo "###########"
	date
	if netstat -tupln 2> /dev/null |grep ssh|grep -q "${PORT}"
	then
		echo "SSH connection appears to still be alive"
	else
		echo "SSH connection appears down, restating it"
		ssh -fnNT -D "${PORT}" "${HOST}"
	fi
	echo "Waiting for ${WAITING_TIME}"
	sleep "${WAITING_TIME}"
done
