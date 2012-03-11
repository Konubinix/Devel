#!/bin/bash

notmuch search --output=files tag:deleted | while read file
do
	rm "$file"
done
