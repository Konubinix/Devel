#!/bin/bash

PROGRAM_NAME="$1"
shift
python "$@" "$(which "${PROGRAM_NAME}")"
