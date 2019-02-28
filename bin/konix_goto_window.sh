#!/bin/bash
# Author: Lucas van Staden (lvs at dedmeet.com / www.dedmeet.com)
# Edited by: Konubinix
# This little script will try and find the application attempting to start
# in the running processes, and if found, focus the application
# if not found, a new instance will start
# 1 - windows name pattern to match
# 2 - application to call if the windows pattern is not found (defaults to $1)

WMCTRL=`which wmctrl`;
GREP=`which grep`;
WINDOW_MATCH="$1"
FALLBACK="$2"
if [ "$FALLBACK" == "" ]
then
	FALLBACK="$WINDOW_MATCH"
fi

# konix_display.py "goto $WINDOW_MATCH " &
shift 2
ARGS="$@"
BASENAME=`basename $WINDOW_MATCH`;
BASENAME=`echo $BASENAME | tr "[:upper:]" "[:lower:]"`
FOUND=0;
function findwindow {
# 1 = BASENAME
# 2 = WMCTRL
# 3 = GREP
        IFS=$'\n';
        for RUNNING in `$2 -l -x`
        do
                if [ `echo $RUNNING | tr "[:upper:]" "[:lower:]" | $3 -c $WINDOW_MATCH` -gt 0 ]
                then
                        HOSTNAME=`hostname`
                        WINDOW=${RUNNING#*${HOSTNAME} }
                        WINDOW=${WINDOW#*N/A }
                        $2 -a $WINDOW
                        FOUND=1;
                fi;
        done
}
findwindow $BASENAME $WMCTRL $GREP;

if [ $FOUND -eq 0 ]
then
    # $ARGS do not give the args till I find a way to get shift and $*
    # or $@ working
    konix_launch_app.sh "$FALLBACK"
    sleep 2;
    findwindow $BASENAME $WMCTRL $GREP;
    if [ $FOUND -eq 0 ]
    then
        sleep 3;
        findwindow $BASENAME $WMCTRL $GREP;
    fi
fi
