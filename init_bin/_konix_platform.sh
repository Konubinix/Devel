#!/bin/bash

(
	cd "${HOME}/init_bin"
	if echo "$OSTYPE" | "./konix_match_regexp.py" -q "linux"
	then
		echo "linux"
	elif echo "$OSTYPE" | "./konix_match_regexp.py" -q "darwin"
	then
		echo "darwin"
	elif echo "$OSTYPE" | "./konix_match_regexp.py" -q -i "wind"
	then
		echo "win32"
	elif echo "$OSTYPE" | "./konix_match_regexp.py" -q -i "cygw"
	then
		echo "cygwin"
	fi
)
