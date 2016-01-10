#!/bin/bash

set -eu

usage () {
    cat<<EOF
$0 -d DELIMITER -l LANG -n NUMBER -r REGEXP_FILTER
EOF
}

NUMBER=3
LANG_=fr
DELIMITER=""
REGEXP_FILTER="^[a-zA-Z0-9_-]\+$"
while getopts "hn:l:d:r:" opt; do
	case $opt in
		h)
			usage
			exit 0
			;;
		n)
			NUMBER="$OPTARG"
			;;
		l)
			LANG_="$OPTARG"
			;;
		d)
			DELIMITER="$OPTARG"
			;;
		d)
			DELIMITER="$OPTARG"
			;;
	esac
done
shift $((OPTIND-1))

aspell dump master -l "$LANG_" | grep "${REGEXP_FILTER}" | shuf -n "${NUMBER}" | paste -s - -d "${DELIMITER}"
