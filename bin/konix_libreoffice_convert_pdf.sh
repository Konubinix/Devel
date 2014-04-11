#!/bin/bash

FILE="$1"
OUTDIR="$(dirname "${FILE}")"
soffice --headless --convert-to pdf --outdir "$OUTDIR" "$FILE"
