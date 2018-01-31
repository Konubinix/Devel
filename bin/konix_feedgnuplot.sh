#!/bin/bash

# gnuplot wants to be kill twice to exit properly
trap "kill %1 ; kill %1 ; exit 0" INT

{
    if [ -e plot.data ]
    then
        cat plot.data
    fi
    while read line
    do
        echo "$(date +"%Y/%m/%d-%H:%M:%S") ${line}"
    done
} | \
    tee plot.data | \
    feedgnuplot --lines --stream --timefmt '%Y/%m/%d-%H:%M:%S' --xlabel 'Time' --domain "$@" &
wait %1
