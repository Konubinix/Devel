#!/bin/bash

set -eu
LINE_NUMBER="$1"

if echo "${LINE_NUMBER}"| grep -q "-"
then
    LINE_NUMBER="${LINE_NUMBER/-/}"
    tail -n "${LINE_NUMBER}" | head -n 1
else
    head -n "${LINE_NUMBER}" | tail -n 1
fi
