#!/bin/bash

ls -S|head -40|while read line
do
	echo "$line"
	notmuch search id:$(sed -n -r 's/[mM]essage-[iI][dD]: <(.+)>/\1/p' "${line}")
done
