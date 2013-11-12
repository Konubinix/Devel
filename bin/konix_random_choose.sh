#!/bin/bash

TEMP_FILE=`mktemp`
trap "rm '$TEMP_FILE'" 0

cat > "${TEMP_FILE}"
NB_LINES=`cat "$TEMP_FILE"|wc -l`
POS=`echo "$RANDOM*$NB_LINES/32767+1"|bc`
head -n $POS "${TEMP_FILE}"|tail -n 1
