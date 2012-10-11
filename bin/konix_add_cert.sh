#!/bin/bash

echo "# $@" >> "${KONIX_CA_FILE}"
konix_dump_cert.sh "$@" >> "${KONIX_CA_FILE}"
