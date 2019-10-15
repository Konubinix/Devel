#!/bin/bash

HOUR="$(date +%H)"
MONTH="$(date +%m)"

if [ ${MONTH} -gt 4 ] && [ ${MONTH} -lt 10 ]
then
    ## summer
    [ ${HOUR} -ge 7 ] && [ ${HOUR} -lt 21 ]
else
    ## winter
    [ ${HOUR} -ge 8 ] && [ ${HOUR} -lt 19 ]
fi
