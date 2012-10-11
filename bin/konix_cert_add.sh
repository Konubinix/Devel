#!/bin/bash

echo "# $@" >> "${KONIX_CA_FILE}"
konix_cert_dump.sh "$@" >> "${KONIX_CA_FILE}"
