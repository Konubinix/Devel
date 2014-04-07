#!/bin/bash

source "git-annex-perso_lib.sh"

usage () {
    cat<<EOF
$0 -h -k

-h: attempt to clean not known remotes
EOF
}


while getopts "hk" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        k)
            export GITANNEXPERSO_REMOTECLEAN=1
            ;;
    esac
done
shift $((OPTIND-1))

REMOTE="$1"

gaps_log "Freezing before fixing remotes"
git-annex-perso_freeze.sh || gaps_error_n_quit "Failure when freezing"
gaps_log "Fixing remotes"
gaps_remotes_fix "${REMOTE}"
