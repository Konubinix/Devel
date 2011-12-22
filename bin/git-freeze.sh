#!/bin/bash
#set -x
print () {
	echo "$*" >&2
}
print "# Adding tracked files"
git add -u -v
print "# Removing deleted files"
git ls-files --deleted | while read FILE
do
	print "Deleting $FILE"
	git rm -f "$FILE"
done
print "# Adding untracked files"
git ls-files -o | while read FILE
do
	if [ -s "$FILE" ]
	then
		print "Adding $FILE"
		git add "$FILE"
	else
		print "Skipping the addition of $FILE because it is empty"
	fi
done
git commit -m "Freezing of repo by $LOGNAME at $HOSTNAME"
