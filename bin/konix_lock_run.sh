#!/bin/bash

usage () {
    cat<<EOF
$0 [-h] [-n] -N name command...
h: show this
n: non blocking script
name: name of the lock
EOF
}

while getopts "nhN:" opt; do
	case $opt in
		h)
			usage
			exit 0
			;;
		n)
			FLOCK_ARGS="${FLOCK_ARGS} -n"
			;;
		N)
			NAME="${OPTARG}"
			;;
	esac
done
[ -n "${NAME}" ] || { echo "You must provide a name" >&2 ; usage ; exit 127 ; }
shift $((OPTIND-1))

(
    flock ${FLOCK_ARGS} 9 || { echo "A script associated to ${NAME} is already running" >&2 ; exit 128 ; }
    "$@"
    echo "Command ${*} ended" 2>&1
) 9>"/var/lock/${NAME}.lock"
