#!/bin/bash

INPUT="${1}"
EXT=".${INPUT##*.}"
TMP_FILE="$(mktemp --suffix "${EXT}")"

convert "${INPUT}" -auto-orient "${TMP_FILE}" && mv "${TMP_FILE}" "${INPUT}"
