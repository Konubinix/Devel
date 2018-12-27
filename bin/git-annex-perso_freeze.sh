#!/bin/bash

set -eux
source "git-annex-perso_lib.sh"

gaps_log "Re run the config in case it changed"
gaps_launch_config
# this is a general configuration that avoid a lot of trouble when almost
# running out of disk space
git -c core.bare=false config annex.diskreserve 50M
git -c core.bare=false config annex.genmetadata true

gaps_log "Freezing the repo"
git-annex-freeze.sh "$@"
git-annex-freeze-views.sh
