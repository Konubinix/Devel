#!/bin/bash

source _konix_sync_variables.sh

konix_sync_select_secondary_dir () {
    read res

    if [[ $res != [0-9]* ]]
    then
	echo "$res is not a number"
	exit 1
    fi
    NUMBER_OF_LINES="$(wc -l "$KONIX_SYNC_DIR_FILE"|cut -f 1 -d' ')"
    if [ $res -gt $NUMBER_OF_LINES ]
    then
	echo "Result $res is greater than the number of directories : $NUMBER_OF_LINES"
	exit 2
    fi
    if [ $res -le 0 ]
    then
	echo "Result $res must be upper than 1"
	exit 3
    fi
}
