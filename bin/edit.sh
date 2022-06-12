#!/bin/bash

if emacsclient -a "failed" --eval '(message "OK")' > /dev/null 2>&1
then
    e "$@"
else
    vim "$@"
fi
