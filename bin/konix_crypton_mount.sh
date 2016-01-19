#!/bin/bash

set -ue
CRYPTON="${KONIX_PERSO_DIR}/crypton"
CRYPTOFF="${HOME}/cryptoff"
mkdir -p "${CRYPTOFF}"
encfs "${CRYPTON}" "${CRYPTOFF}"
