#! /bin/bash

#set -x

if [ -z "$1" ]
then
	echo "one arg if needed"
	exit 1
fi

if [ -e "$(which $1)" ]
then
	last="$(which $1)"
elif [ -e "$1" ]
then
	last="$1"
else
	echo "Cannot find a file corresponding to $1 (neither in the path nor in the current working directory)" >&2
	exit 1
fi

read_link()
{
	if [ ! -e "$1" ]
	then
		if which "$1" > /dev/null 2>&1
		then
			LINK="$(which "$1")"
		else
			echo "Cannot find any file for $1" >&2
			return 1
		fi
	else
		LINK="$1"
	fi
	res="$(konix_readlink.sh "$LINK")"
	rc=$?
	if [ "$rc" != "0" ]
	then
		return $rc
	fi
	if echo "$res" |grep -q "^\."
	then
		dir_name="$(dirname "$LINK")"
		res="$dir_name/$res"
	fi
	echo "$res"
}

tmp="$(read_link "$last")"
res=$?
while [ "$res" == "0" ]
do
	last="$tmp"
	tmp="$(read_link "$last")"
	res=$?
done
if [ "$res" == "2" ]
then
	echo "$last"
fi
