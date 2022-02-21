#!/bin/bash

set -eu
PID=$$

while [ "${PID}" != "1" ]
do
    PID="$(ps -p "$PID" -o ppid=|trim)"
    if readlink /proc/$PID/exe | grep -q tmux
    then
        exit 0
    fi
done
exit 1
