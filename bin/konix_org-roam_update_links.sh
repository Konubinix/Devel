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
OUTPUT_DIR="$2"
DB="$3"
KIND="$4"

konix_org-roam_exported_kind.sh "${ROAM_DIR}" "${KIND}" | sort > "${OUTPUT_DIR}/org-roam_exported_kind.txt"
konix_org-roam_exported.sh "${ROAM_DIR}" | sort > "${OUTPUT_DIR}/org-roam_exported.txt"
konix_org-roam_get_exported_links.py "${DB}" "${ROAM_DIR}" "${OUTPUT_DIR}/org-roam_exported_kind.txt" | sort > "${OUTPUT_DIR}/org-roam_links_kind.txt"
konix_org-roam_get_exported_links.py "${DB}" "${ROAM_DIR}" "${OUTPUT_DIR}/org-roam_exported.txt" | sort > "${OUTPUT_DIR}/org-roam_links.txt"