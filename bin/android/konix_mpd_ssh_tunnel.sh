#!/bin/bash

set -eu
exec ssh -f -L9635:${MPD_HOST}:9635 -L6660:${MPD_HOST}:6600 "${MPD_HOST_TUNNELER}" sleep 5000000000
