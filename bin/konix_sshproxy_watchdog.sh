#!/bin/bash

set -eu

usage () {
    cat<<EOF
$0 -p PORT -h HOST [-w WAITING_TIME]

Starts a local proxy on port PORT tunnelling traffic through ssh connection with
HOST

Once a connection is broken, the program will wait for WAITING_TIME seconds
(defaults to 10) before trying again.
EOF
}

WAITING_TIME="10"
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
if netstat -tupln 2> /dev/null |grep -q "${PORT}"
then
	echo "There is already something listening on port ${PORT}"
fi

while true
do
	echo "###########"
	date
	ssh -nNT -D "${PORT}" "${HOST}"
	echo "Waiting for ${WAITING_TIME}"
	sleep "${WAITING_TIME}"
done
