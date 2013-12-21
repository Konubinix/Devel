#!/bin/bash

FILE="$1"
shift 1

ABS_PATH="$(readlink "$FILE")"
DIR="$(dirname "${ABS_PATH}")"
NAME="$(basename "${ABS_PATH}")"
cd "${DIR}"

"$@" "${NAME}"
