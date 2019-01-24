#!/bin/bash

HOUR="$(date +%H)"
MINUTE="$(date +%M)"

( [ ${HOUR} -eq 12 ] && [ ${MINUTE} -ge 30 ] ) \
    || \
    ( [ ${HOUR} -eq 13 ] )
