#!/bin/bash
#set -x
print () {
	echo "$*" >&2
}
print "Adding tracked files"
git add -u
print "Removing deleted files"
git ls-files --deleted | while read FILE
do
	git rm -f "$FILE"
done
print "Adding untracked files"
git ls-files -o | while read FILE
do
	git add "$FILE"
done
git commit -m "Freezing repo"
