#!/bin/bash


TAGS_INCLUDES_FILE="./TAGS_INCLUDE"
if [ ! -f "$TAGS_INCLUDES_FILE" ]
then
	echo "$TAGS_INCLUDES_FILE must exist" >&2
	exit 1
fi

IFS=$'\n'
for INCLUDE_FILE in $(<$TAGS_INCLUDES_FILE)
do
	INCLUDE_DIR="$(dirname "$INCLUDE_FILE")"
	cat<<EOF
##################################################
# UPDATING THE INCLUDE DIR $INCLUDE_DIR
##################################################
EOF
	(
		cd "$INCLUDE_DIR"
		konix_etags_create.sh
	)
done
