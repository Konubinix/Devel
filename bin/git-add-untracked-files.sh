#!/bin/bash

print () {
	echo "$*" >&2
}

git ls-files -o --exclude-standard | while read FILE
do
	if [ -s "$FILE" ]
	then
		print "Adding $FILE"
		git add "$FILE"
	else
		print "Skipping the addition of $FILE because it is empty"
	fi
done
