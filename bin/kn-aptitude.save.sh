#!/bin/bash

if [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

set -eux
DIR="${KONIX_PERSO_DIR}/${HOSTNAME}/aptitude_nd"
mkdir -p "${DIR}"
aptitude search '~i ?not(~M)' > "${DIR}/list"
cp "/var/lib/aptitude/pkgstates" "${DIR}/state"
cp "/var/log/aptitude" "${DIR}/log"
