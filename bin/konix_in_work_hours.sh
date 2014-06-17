#!/bin/bash

WEEK_NUMBER="$(date +%u)"
HOUR="$(date +%H)"

[ ${WEEK_NUMBER} -gt 0 ] \
    &&  [ ${WEEK_NUMBER} -lt 6 ] \
    &&  [ ${HOUR} -gt 8 ] \
    &&  [ ${HOUR} -lt 17 ]
