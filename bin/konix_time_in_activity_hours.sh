#!/usr/bin/env bash

HOUR="$(date +%H)"

[ ${HOUR} -ge 8 ] \
    &&  [ ${HOUR} -lt 19 ]
