#!/bin/bash -eux

rm -f "${KONIX_BACKGROUND_IMAGE}"
konix_patchwork.py "${KONIX_BACKGROUND_DIR}"/* "${KONIX_BACKGROUND_IMAGE}"
