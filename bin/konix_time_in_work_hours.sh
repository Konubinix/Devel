#!/bin/bash

HOUR="$(date +%H)"

[ ${HOUR} -gt 8 ] \
    &&  [ ${HOUR} -lt 17 ]
