#!/bin/bash

source "git-annex-perso_lib.sh"

gaps_log "Re run the config in case it changed"
gaps_launch_config
# this is a general configuration that avoid a lot of trouble when almost
# running out of disk space
git config annex.diskreserve 50M

gaps_log "Freezing the repo"
git-annex-freeze.sh
