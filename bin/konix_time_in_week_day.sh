#!/usr/bin/env bash

WEEK_NUMBER="$(date +%u)"

[ ${WEEK_NUMBER} -gt 0 ] \
    &&  [ ${WEEK_NUMBER} -lt 6 ]
