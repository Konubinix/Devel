#!/bin/bash
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

TMP="$(mktemp -d)"
trap "rm -rf '${TMP}'" 0

cat > "${TMP}/in"

# use xsel instead of xclip, see
# https://unix.stackexchange.com/questions/316715/xclip-works-differently-in-interactive-and-non-interactive-shells

xsel --input --primary < "${TMP}/in"
xsel --input --secondary < "${TMP}/in"
xsel --input --clipboard < "${TMP}/in"
