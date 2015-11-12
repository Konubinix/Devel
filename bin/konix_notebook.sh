#!/bin/bash

set -eu
cd "${KONIX_NOTEBOOKS_DIR}"
notebook --ip "${KONIX_NOTEBOOK_IP}" --port "${KONIX_NOTEBOOK_PORT}" --no-browser "${@}"
