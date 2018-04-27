#!/bin/bash

if [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

export LC_ALL=C

aptitude -O installsize search ~i -F '%I | %p' --disable-columns|sed -r 's/^([0-9]+) ([a-zA-Z]+)(.*)$/\1\2\3/'|sort -h
