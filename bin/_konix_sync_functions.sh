#!/bin/bash

source _konix_sync_variables.sh

konix_sync_select_secondary_dir () {
    read RES

    if [[ $RES != [0-9]* ]]
    then
	echo "$RES is not a number">&2
	exit 1
    fi
    NUMBER_OF_LINES="$(wc -l "$KONIX_SYNC_DIR_FILE"|cut -f 1 -d' ')"
    if [ $RES -gt $NUMBER_OF_LINES ]
    then
	echo "RESult $RES is greater than the number of directories : $NUMBER_OF_LINES">&2
	exit 2
    fi
    if [ $RES -le 0 ]
    then
	echo "RESult $RES must be upper than 1" >&2
	exit 3
    fi
}

konix_sync_pwd () {
	RES="$(konix_add_trailing_slash.py "$(pwd)")"
}
