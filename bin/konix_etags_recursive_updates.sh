#!/bin/bash

source _konix_etags_variables.sh

TAGS_INCLUDES_FILE="./$TAGS_INCLUDES_FILE_NAME"
PWD=`pwd`
cat<<EOF
# #################################################
# Recursively update $PWD
# #################################################
EOF

if [ -f "$TAGS_INCLUDES_FILE" ]
then
	IFS=$'\n'
	for INCLUDE_FILE in $(<$TAGS_INCLUDES_FILE)
	do
		INCLUDE_DIR="$(dirname "$INCLUDE_FILE")"
		(
			cd "$INCLUDE_DIR"
			"$0"
		)
	done
fi
cat<<EOF
# ####################################################################################################
# Update current dir $(pwd)
# ####################################################################################################
EOF
konix_etags_create.sh -v
