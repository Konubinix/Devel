#!/bin/bash

for disk in /media/*
do
	disk_name=`basename "$disk"`
	# avoid the excluded ones
	if [ -e "$KONIX_DB_MEDIA_EXCLUDE" ] && grep -q "^$disk_name\$" "${KONIX_DB_MEDIA_EXCLUDE}"
	then
		continue
	fi
	if [ -d "$KONIX_DB_MEDIA_DBS" ]
	then
		mkdir -p "$KONIX_DB_MEDIA_DBS"
	fi
	updatedb -l 0 -U "$disk" -o "$KONIX_DB_MEDIA_DBS/${disk_name}.db"
done
echo "Done"
