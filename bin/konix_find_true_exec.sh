#!/bin/bash
if [ -z "$1" ]
then
    echo "ONE ARGUMENT REQUIRED"
    exit 1
fi

real_file="$(konix_readlink_last.sh "$1")"
if [ "$?" == "0" ]
then
	basename "$real_file"
fi
