#!/bin/bash

FILE="${1}"
shift
exec yEd "$@" "$(konix_absolute_path.py "${FILE}")"
