#!/bin/bash

eval "[ $* ]"
if [ "$?" != "0" ]
then
	echo "Assert on condition $*"
	frame_number=0
	while caller $frame_number
	do
		frame_number=$((frame_number + 1))
	done
	exit 1
fi
