#! /bin/bash
#set -x

if [ -z "$1" ]
then
	echo "one arg if needed"
	exit 1
fi

elem="$1"

line="$(ls -l "$elem" 2>&1)"
res=$?
if [ "$res" != "0" ]
then
	exit 1
fi
if echo "$line" |grep -q "\->"
then
	echo "$line"|awk '/->/ {print $11}'
else
	exit 2
fi
