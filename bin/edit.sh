#!/bin/bash

if emacsclient -a "failed" --eval '(message "OK")' > /dev/null 2>&1
then
    ec "$@"
else
    vim "$@"
fi
