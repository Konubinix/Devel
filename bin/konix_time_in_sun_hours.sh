#!/bin/bash

HOUR="$(date +%H)"

[ ${HOUR} -ge 7 ] \
    &&  [ ${HOUR} -lt 21 ]
