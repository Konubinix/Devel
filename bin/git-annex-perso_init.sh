#!/bin/bash

source "git-annex-perso_lib.sh"

log ( ) {
    echo "## $*"
}

if [ "$1" == "" ]
then
	echo "You must provide a description" >&2
	exit 2
fi

gaps_source_script_maybe ".gitannexinit_prehook"

log "Initing the repo with description '$*'"
git init
log "Initing the annex"
git annex init "$*"

log "Setting up the post hook"
post_hook_launch ( ) {
	gaps_source_script_maybe ".gitannexinit_posthook"
}
trap "post_hook_launch" 0

git-annex-perso_initremotes.sh
gaps_launch_config
