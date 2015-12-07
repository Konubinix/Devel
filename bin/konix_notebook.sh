#!/bin/bash

set -eu
mkdir -p "${KONIX_NOTEBOOKS_DIR}"
cd "${KONIX_NOTEBOOKS_DIR}"
notebook --ip "${KONIX_NOTEBOOK_IP}" --port "${KONIX_NOTEBOOK_PORT}" --no-browser "${@}"
