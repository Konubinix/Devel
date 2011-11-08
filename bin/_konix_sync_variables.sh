#!/bin/bash
if [ "$KONIX_SYNC_ALREADY_SOURCED" == "" ]
then
    KONIX_SYNC_DIR_FILE=".konix_sync_dirs"
    KONIX_SYNC_MAIN_DIR_FILE=".konix_sync_main_dir"
    KONIX_SYNC_EXCLUDE_FILE=".konix_sync_exclude"
    KONIX_SYNC_INCLUDE_FILE=".konix_sync_include"
    KONIX_SYNC_FILES_FILE=".konix_sync_files"
    KONIX_SYNC_PUSH_PRE_HOOK=".konix_sync_push_pre_hook"
    KONIX_SYNC_PULL_POST_HOOK=".konix_sync_pull_post_hook"
    KONIX_SYNC_LAST_PUSHER_FILE=".konix_sync_last_pusher"
	KONIX_SYNC_SYNCER=".konix_sync_syncer"
    KONIX_SYNC_MY_NAME="$(uname -n)"
    if [ -e "$KONIX_SYNC_FILES_FILE" ]
    then
	FILES_ARG="--files-from=${KONIX_SYNC_FILES_FILE}"
    else
	FILES_ARG=""
	if [ -e "$KONIX_SYNC_EXCLUDE_FILE" ]
	then
	    RSYNC_EXCLUDE_ARGS="--exclude-from=${KONIX_SYNC_EXCLUDE_FILE}"
	else
	    RSYNC_EXCLUDE_ARGS=""
	fi
	if [ -e "$KONIX_SYNC_INCLUDE_FILE" ]
	then
	    RSYNC_INCLUDE_ARGS="--include-from=${KONIX_SYNC_INCLUDE_FILE}"
	else
	    RSYNC_INCLUDE_ARGS=""
	fi
    fi
    RSYNC_ARGS="-rucz --delete --progress --exclude ${KONIX_SYNC_DIR_FILE} --exclude ${KONIX_SYNC_MAIN_DIR_FILE} --exclude ${KONIX_SYNC_EXCLUDE_FILE} $FILES_ARG $RSYNC_INCLUDE_ARGS $RSYNC_EXCLUDE_ARGS"
    KONIX_SYNC_PWD="$(konix_add_trailing_slash.py "$(pwd)")"
    KONIX_SYNC_ALREADY_SOURCED=1
fi
