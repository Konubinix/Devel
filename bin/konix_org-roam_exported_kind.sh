#!/bin/bash
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

ROAM="$1"
KIND="$2"
grep -r -l "^#+KONIX_ORG_PUBLISH_KIND: ${KIND}" "${ROAM}"
