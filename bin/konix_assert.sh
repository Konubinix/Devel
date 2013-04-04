#!/bin/bash

show_context () {
    FILE_NAME=$1
    LINE_NUMBER=$2
    CONTEXT=$3
    cat -n "$FILE_NAME" \
        | head -$((LINE_NUMBER + CONTEXT)) \
        | tail -$((2 * CONTEXT)) \
        | sed "/^ \+$LINE_NUMBER\t/ {
    s/^ />/
}"
}

eval "[ $* ]"
if [ "$?" != "0" ]
then
 	echo "Assert on condition '$*'"
	frame_number=0
	while caller $frame_number > /dev/null
	do
        caller $frame_number \
            | sed 's/^\(.\+\) \(.\+\) \(.\+\)$/>>>> called by \2 at \3:\1/'
        line_number=$(caller $frame_number|cut -f 1 -d' ')
        file_name=$(caller $frame_number|cut -f 3 -d' ')
        show_context $file_name $line_number 3
		frame_number=$((frame_number + 1))
	done
	exit 1
fi
