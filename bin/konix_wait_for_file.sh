#!/bin/bash

while [ ! -e "$1" ]
do
	echo "file $1 does not exist, retry in 1 second"
    sleep 1
done
echo "file $1 exists, going on"
