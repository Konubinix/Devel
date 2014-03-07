#!/bin/bash

FILE="${1}"
REAL_FILE="`readlink "${FILE}"`"
ANNEX_TMP="`git-toplevel.sh`/.git/annex/tmp"
OPEN="mimeopen -n"
trap "kill %1" 0
if [ -e "${REAL_FILE}" ]
then
	exec $OPEN "${REAL_FILE}"
else
	git annex get "${FILE}" & 2>&1 > /dev/null
	FILE_SHA="`basename "${REAL_FILE}"`"
	FILE_TO_OPEN="${ANNEX_TMP}/${FILE_SHA}"
	while ! [ -e "${FILE_TO_OPEN}" ]
	do
		if ! jobs %1 2>&1 > /dev/null
		then
			echo "Failed to get access to the file..."
			exit 1
		fi
		echo "Waiting for ${FILE_TO_OPEN} to be present" >&2
		sleep 1
	done
	$OPEN "${FILE_TO_OPEN}"
fi
