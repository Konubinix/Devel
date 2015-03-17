#!/bin/bash

INPUT="${1}"

if exiftool -p '$Orientation' "${INPUT}"|grep -q "Horizontal"
then
    echo "Already Horizontal"
    exit 0
fi

EXT=".${INPUT##*.}"
TMP_FILE="$(mktemp --suffix "${EXT}")"

convert "${INPUT}" -auto-orient "${TMP_FILE}" && mv "${TMP_FILE}" "${INPUT}"
