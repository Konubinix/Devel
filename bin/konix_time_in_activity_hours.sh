#!/bin/bash

HOUR="$(date +%H)"

[ ${HOUR} -ge 8 ] \
    &&  [ ${HOUR} -lt 19 ]
