#!/bin/bash

while [ $# -gt 0 ]
do
    find ${HOME}/Mail/ -maxdepth 2 -name "$1"
    shift
done | while read folder
do
    if ! [ -e "$folder" ]
    then
        echo "$folder does not exist..." >&2
        continue
    fi
    echo "${folder}"|sed "s|${HOME}/Mail/||"
done
