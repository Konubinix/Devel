#!/bin/bash

if [ -d "$KONIX_DB_MEDIA_DBS" ]
then
	DBS=`echo $KONIX_DB_MEDIA_DBS/*.db`
	DBS="${DBS// /:}"
else
	echo "KONIX_DB_MEDIA_DBS shoudl be set">&2
	exit 1
fi
if [ -z "$DBS" ]
then
	echo "Nothing found in $KONIX_DB_MEDIA_DBS">&2
	exit 1
fi
locate -d "$DBS" "$@"
