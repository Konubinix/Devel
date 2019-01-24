#!/bin/bash

HOUR="$(date +%H)"
MINUTE="$(date +%M)"

( [ ${HOUR} -eq 11 ] && [ ${MINUTE} -ge 30 ] ) \
    || \
    ( [ ${HOUR} -eq 12 ] && [ ${MINUTE} -le 30 ] )
