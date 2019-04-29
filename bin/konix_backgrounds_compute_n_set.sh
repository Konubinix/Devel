#!/bin/bash -eux

konix_backgrounds_compute.sh
feh --bg-scale ${KONIX_BACKGROUND_IMAGE_PREFIX}*
