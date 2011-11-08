#!/bin/bash

if echo "$OSTYPE" | "${HOME}/init_bin/konix_match_regexp.py" -q "linux"
then
	echo "linux2"
elif echo "$OSTYPE" | "${HOME}/init_bin/konix_match_regexp.py" -q "darwin"
then
	echo "darwin"
elif echo "$OSTYPE" | "${HOME}/init_bin/konix_match_regexp.py" -q -i "win"
then
	echo "win32"
fi
