#!/bin/bash -x

syncplayClient.py --no-gui \
				  -a "${KONIX_SYNCPLAY_SERVER}" \
				  -r "${KONIX_SYNCPLAY_ROOM}" \
				  -n "${KONIX_SYNCPLAY_NAME}" \
				  --player-path=/usr/bin/mplayer "$@" -- ${KONIX_SYNCPLAY_EXTRA_CONFIG}
