#!/bin/bash

source _konix_etags_variables.sh

TAGS_INCLUDES_FILE="./$TAGS_INCLUDES_FILE_NAME"

if [ -f "$TAGS_INCLUDES_FILE" ]
then
	IFS=$'\n'
	for INCLUDE_FILE in $(<$TAGS_INCLUDES_FILE)
	do
		INCLUDE_DIR="$(dirname "$INCLUDE_FILE")"
		cat<<EOF
# #################################################
# Recursively update the include dir $INCLUDE_DIR
# #################################################
EOF
		(
			cd "$INCLUDE_DIR"
			konix_etags_includes_updates.sh
		)
	done
fi
cat<<EOF
# ####################################################################################################
# Update current dir $(pwd)
# ####################################################################################################
EOF
konix_etags_create.sh -v
