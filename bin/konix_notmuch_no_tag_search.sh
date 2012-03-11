#!/bin/bash

TAGS="$(notmuch search --output=tags -- '*')"
TAGS_LINE="not tag:dummy"
IFS=$'\n'
for tag in $TAGS
do
	TAGS_LINE="$TAGS_LINE and not tag:$tag"
done
echo $TAGS_LINE
