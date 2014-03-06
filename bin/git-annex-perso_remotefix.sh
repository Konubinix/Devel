#!/bin/bash

source "git-annex-perso_lib.sh"

REMOTE="$1"

gaps_log "Freezing before fixing remotes"
git-annex-perso_freeze.sh || gaps_error_n_quit "Failure when freezing"
gaps_log "Fixing remotes"
gaps_remotes_fix "${REMOTE}"
