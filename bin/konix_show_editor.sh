#!/bin/bash

TMP_FILE=`mktemp`
trap "rm '$TMP_FILE'" 0
tee "$TMP_FILE" > /dev/null
"$EDITOR" "$TMP_FILE"
