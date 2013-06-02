#!/bin/bash

EXEC="$1"

usage ( ) {
	cat<<EOF
$0 command
EOF
}

die ( ) {
	message="$1"
	echo "$message"
	usage
	exit 1
}

EXEC_PATH="$(which "${EXEC}")"
[ "$?" == "0" ] || die "${EXEC} could not be found"
shift
sudo "$(which ${EXEC})" "$@"
