#!/bin/bash

source _konix_sync_variables.sh

REMOTE_DIR="$*"
if [ ! -e "$REMOTE_DIR" ]
then
    echo "$REMOTE_DIR does not exist, create it ? (y/.)"
    read res
    if [ "$res" == "y" ]
    then
	mkdir -p "$REMOTE_DIR"
    else
	echo "cannot go further without making the sync directory" >&2
	exit 1
    fi
fi

echo "$REMOTE_DIR" >> "$KONIX_SYNC_DIR_FILE"
echo "Added $REMOTE_DIR to sync directories"
