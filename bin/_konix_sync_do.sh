#!/bin/bash
source _konix_sync_variables.sh
ACTION="$1"

usage () {
	cat<<EOF
$(pwd)/$KONIX_SYNC_MAIN_DIR_FILE must exist
EOF
}

if [ ! -e "$KONIX_SYNC_MAIN_DIR_FILE" ]
then
	usage
	exit 1
fi

get_last_pusher () {
	local REMOTE_FILE_NAME="$1$KONIX_SYNC_LAST_PUSHER_FILE"
	if [ -e "$REMOTE_FILE_NAME" ]
	then
		local LAST_PUSHER="$(cat $REMOTE_FILE_NAME)"
	else
		return 1
	fi
	RES="$LAST_PUSHER"
}

last_pusher_was_me_p () {
	get_last_pusher "$1"
	local LAST_PUSHER="$RES"
	if [ "$LAST_PUSHER" == "$KONIX_SYNC_MY_NAME" ]
	then
		return 0
	else
		return 1
	fi
}

check_push () {
	local REMOTE="$1"
	if ! last_pusher_was_me_p "$REMOTE"
	then
		get_last_pusher "$1"
		if [ "$?" == "0" ]
		then
			local LAST_PUSHER="$RES"
			echo "Last push was not from here, do it anyway (you may loose info from $LAST_PUSHER)? "
			read res
			if [ "$res" == "y" ]
			then
				return 0
			else
				return 1
			fi
		else
			return 0
		fi
	else
		return 0
	fi
}

check_pull () {
	local REMOTE="$1"
	if last_pusher_was_me_p "$REMOTE"
	then
		echo "Here is where the lat push was done, it is not needed to pull, pull anyway ?"
		read res
		if [ "$res" == "y" ]
		then
			return 0
		else
			return 1
		fi
	fi
}

check_push_pull () {
	local REMOTE="$1"
	if [ "$ACTION" == "PUSH" ]
	then
		check_push "$REMOTE"
	else
		check_pull "$REMOTE"
	fi

}

set_last_pusher () {
	echo "$KONIX_SYNC_MY_NAME" > "$1$KONIX_SYNC_LAST_PUSHER_FILE"
}

check_may_sync () {
    # ARGS : LOCAL REMOTE
	for ELEM in "$1" "$2"
	do
		if [ ! -e "$ELEM" ]
		then
			echo "$ELEM does not exist"
			return 1
		fi
	done
	if ! check_push_pull "$2"
	then
		echo "Skipping action for current sync"
		return 1
	fi
	return 0
}

set_sync_command () {
	FROM="$1"
	TO="$2"
	if [ -e "$KONIX_SYNC_SYNCER" ]
	then
		TYPE_OF_SYNC=$(< "$KONIX_SYNC_SYNCER")
	else
		TYPE_OF_SYNC="git-sync"
	fi
	case $TYPE_OF_SYNC in
		rsync*)
			SYNC_COMMAND="rsync ${RSYNC_ARGS}"
			;;
		git-sync*)
			SYNC_COMMAND="git-sync.sh"
			;;
		unison*)
			SYNC_COMMAND="unison"
			;;
		git-priv*)
			SYNC_COMMAND="git-priv-sync.sh"
			;;
		*)
			SYNC_COMMAND="git-sync.sh"
			;;
	esac
	SYNC_COMMAND="$SYNC_COMMAND \"$FROM\" \"$TO\""
}

IFS=$'\n'
MAIN_DIRS="$(<${KONIX_SYNC_MAIN_DIR_FILE})"
for main_dir in $MAIN_DIRS
do
	main_dir="$(konix_add_trailing_slash.py "${main_dir}")"
	DOIT=1
	if [ "$ACTION" == "PUSH" ]
	then
		FROM="$KONIX_SYNC_PWD"
		TO="${main_dir}"
	else
		TO="$KONIX_SYNC_PWD"
		FROM="${main_dir}"
	fi
	if on_windows_p.sh
	then
		FROM="$(cygpath "${FROM}")"
		TO="$(cygpath "${TO}")"
	fi
	FROM="$(konix_add_trailing_slash.py "$FROM")"
	TO="$(konix_add_trailing_slash.py "$TO")"
	echo "Syncing from $FROM to $TO"
	if check_may_sync "$KONIX_SYNC_PWD" "${main_dir}"
	then
		set_sync_command "$FROM" "$TO"
		# this will set the SYNC_COMMAND env variable
		echo "evaluating sync command"
		echo "$SYNC_COMMAND"
		eval $SYNC_COMMAND
		set_last_pusher "${main_dir}"
	fi
done
