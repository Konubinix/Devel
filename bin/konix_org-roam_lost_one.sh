#!/bin/bash
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

DB="$1"
BASE="$2"

TMP="$(mktemp -d)"
trap "rm -rf '${TMP}'" 0

cat "${DB}" | sort > "${TMP}/links.txt"
cat "${BASE}" | sort > "${TMP}/base.txt"

[ "0" != "$(konix_diff_new_lines.sh "${TMP}/links.txt" "${TMP}/base.txt"|wc -l)" ]
