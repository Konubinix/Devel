#!/bin/bash
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

FILE="$1"
TITLE="$(basename "${FILE}")"

urlencode() {
	echo "$*" | konix_urlquote.py
}

ec --eval "(konix/org-add-roam-ref \"file:$(urlencode "${FILE}")\" \"$(urlencode "${TITLE}")\" \"$(xclip -o | konix_urlquote.py)\")"
konix_display.py "Added a roam note about ${FILE}"
