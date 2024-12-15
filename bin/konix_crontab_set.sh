#! /bin/bash

set -eu
if test -e "${KONIX_CRONTAB}"
then
    crontab "${KONIX_CRONTAB}"
fi
