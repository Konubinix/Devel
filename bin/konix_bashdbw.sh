#!/bin/bash

PROGRAM="${1}"
shift

bashdb "$(which "${PROGRAM}")" "$@"
