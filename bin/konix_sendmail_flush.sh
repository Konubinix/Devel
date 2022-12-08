#!/bin/bash -eu

if test 0 = $(msmtpq --manage list | wc -l)
then
    echo "Nothing to send"
else
    TIMETOWAIT=10
    echo "Waiting ${TIMETOWAIT}s before sending, just in case"
    sleep ${TIMETOWAIT}
    echo "Sending now!!"
    msmtpq --manage send
    echo OK
    notmuch new
fi
