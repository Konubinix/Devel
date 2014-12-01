#!/bin/bash

set -eux
DIR="${KONIX_PERSO_DIR}/${HOSTNAME}/notmuch_nd"
mkdir -p "${DIR}"
konix_notmuch_dump_no_automatic_tags.sh > "${DIR}/tag_list"
