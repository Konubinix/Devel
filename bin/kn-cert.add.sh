#!/bin/bash

if [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

echo "# $@" >> "${KONIX_CA_FILE}"
konix_cert_dump.sh "$@" >> "${KONIX_CA_FILE}"
