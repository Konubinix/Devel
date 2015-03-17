#!/bin/bash

FILE="$1"
DIR="$(dirname "${FILE}")"
CONTENT="$2"

if grep -q "$CONTENT" "$FILE"
then
    echo "${DIR}"
    exit 0
else
    exit 1
fi
