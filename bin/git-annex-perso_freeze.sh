#!/bin/bash

source "git-annex-perso_lib.sh"

gaps_log "Re run the config in case it changed"
gaps_launch_config

gaps_launch_freeze_pre_hook "${GITANNEXFREEZE_PRE_HOOK}" "$(pwd)"

gaps_log "Freezing the repo"
git-annex-freeze.sh
