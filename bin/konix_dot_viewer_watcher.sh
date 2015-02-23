#!/bin/bash

set -ue
FILE="$1"
iwatch \
    -e close_write \
    -c "${HOME}/init_bin/konix_do_cron_job.sh konix_dot_viewer_unique_nowait.sh '${FILE}'" \
    "${FILE}"
