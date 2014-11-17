#!/bin/bash

LISTING="$(mktemp)"
trap "rm '${LISTING}'" 0

ls "$@" > "${LISTING}"
head -1 < "${LISTING}"
{ tail -n+2 | head -n-2 | shuf ; } < "${LISTING}"
tail -n2 < "${LISTING}"
