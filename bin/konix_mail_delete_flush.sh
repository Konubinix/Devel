#!/bin/bash

echo "The deleted mails are"
notmuch search tag:deleted
echo "Delete them ?"
read y
if [ "$y" == "y" ]
then
	notmuch search --output=files tag:deleted | while read mail_file
	do
		rm -v "$mail_file"
	done
	echo "Re sync notmuch"
	notmuch new
else
	echo "Doing nothing"
fi
