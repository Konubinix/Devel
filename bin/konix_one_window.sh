#!/bin/bash
# Find_app
# Author: Lucas van Staden (lvs at dedmeet.com / www.dedmeet.com)
# This little script will try and find the application attempting to start
# in the running processes, and if found, focus the application
# if not found, a new instance will start
# usage:
# find_app.sh <application with full path>
# params
# 1 - application to start (full path)
# helper applications
WMCTRL=`which wmctrl`;
GREP=`which grep`;
APPLICATION=$1;
shift
ARGS="$*"
BASENAME=`basename $APPLICATION`;
BASENAME=`echo $BASENAME | tr "[:upper:]" "[:lower:]"`
FOUND=0;
function findwindow {
# 1 = BASENAME
# 2 = WMCTRL
# 3 = GREP
        IFS=$'\n';
        for RUNNING in `$2 -l -x`
        do
                if [ `echo $RUNNING | tr "[:upper:]" "[:lower:]" | $3 -c $1` -gt 0 ]
                then
                        HOSTNAME=`hostname`
                        WINDOW=${RUNNING#*${HOSTNAME} }
                        $2 -R $WINDOW
                        FOUND=1;
                fi;
        done
}
findwindow $BASENAME $WMCTRL $GREP; 
if [ $FOUND -eq 0 ]
then
    $APPLICATION $ARGS
    sleep 2;
    findwindow $BASENAME $WMCTRL $GREP;
    if [ $FOUND -eq 0 ]
    then
        sleep 3;
        findwindow $BASENAME $WMCTRL $GREP;
    fi
fi
