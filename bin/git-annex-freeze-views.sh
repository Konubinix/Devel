#!/bin/bash

source "git-annex-perso_lib.sh"

TOP_LEVEL="$(git-toplevel.sh)"
ANNEX_NAME="$(basename "${TOP_LEVEL}")"
VIEW_PATH="${TOP_LEVEL}/../views_${ANNEX_NAME}"
if [ -d "${VIEW_PATH}" ]
then
    gaps_log "Syncing the view at '${VIEW_PATH}'"
    cd "${VIEW_PATH}"
    if grep -q "view" .git/HEAD
    then
        git-freeze.sh || gaps_log "Did nothing on view"
    else
        gaps_error "The current branch at '${VIEW_PATH}' is not a view"
    fi
fi
