#!/bin/bash -eux
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

FILE="$1"
NAME="$(basename "${FILE}")"
ipfa "${FILE}"
konix_display.py "Done ipfa for ${NAME}"
