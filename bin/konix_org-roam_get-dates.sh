#!/bin/bash
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

ROAM_DIR="$1"

pushd "${ROAM_DIR}"
{
	find "$(pwd)" -type f -name "*.org" -exec konix_org-roam_dump_date.sh '{}' ';'
}
popd
