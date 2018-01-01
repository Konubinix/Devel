#!/bin/bash

source "git-annex-perso_lib.sh"


set -eu
usage ( ) {
    cat<<EOF
$0 [-f] [-n] [-F] [-l] [-d] [-p <PATH>] [-r remote] [-s] [-i]

f: fast
F: force syncs
d: show dates (useful to know what took so long)
r: only sync with this remote
s: skip data transfer
l: long mode, do not give the --fast argument anywhere
i: attempt to fix the remotes before syncing
p: limiting to PATH (default to .), if set to ..., use \$(git-toplevel.sh)
EOF
}

gaps_log_info "####################### IN `pwd`"

export KONIX_GIT_ANNEX_PERSO_SYNC_FAST_ARG="--fast"
gaps_log "#### Parsing arguments"
LIMIT_PATH=""
SYNC_REMOTE=""
KONIX_GIT_ANNEX_PERSO_SYNC_FAST=""
KONIX_GIT_ANNEX_PERSO_SYNC_FAST_ARG=""
KONIX_GIT_ANNEX_PERSO_SYNC_FIX=""
KONIX_GIT_ANNEX_PERSO_SYNC_FORCE=""
KONIX_GIT_ANNEX_PERSO_SHOW_DATES=""
KONIX_GIT_ANNEX_PERSO_SYNC_SKIP_DATA=""
while getopts "hfFdinr:sp:l" opt; do
    case $opt in
        h)
            usage
            exit 1
            ;;
        f)
            KONIX_GIT_ANNEX_PERSO_SYNC_FAST=1
            echo "Fast mode set, will skip big annexes"
            ;;
        l)
            KONIX_GIT_ANNEX_PERSO_SYNC_FAST_ARG=""
            echo "Long mode set, will not use --fast"
            ;;
        i)
            KONIX_GIT_ANNEX_PERSO_SYNC_FIX="1"
            echo "Fix mode set"
            ;;
        F)
            KONIX_GIT_ANNEX_PERSO_SYNC_FORCE=1
            echo "Force mode set, will force data transfer"
            ;;
        d)
            KONIX_GIT_ANNEX_PERSO_SHOW_DATES=1
            echo "Show dates mode set"
            ;;
        r)
            SYNC_REMOTE="${OPTARG}"
            echo "Will only sync with remote ${SYNC_REMOTE}"
            ;;
        p)
            LIMIT_PATH="${OPTARG}"
            ;;
        s)
            KONIX_GIT_ANNEX_PERSO_SYNC_SKIP_DATA=1
            echo "Will skip data transfer"
            ;;
    esac
done
export KONIX_GIT_ANNEX_PERSO_SYNC_FAST
export KONIX_GIT_ANNEX_PERSO_SYNC_FAST_ARG
export KONIX_GIT_ANNEX_PERSO_SYNC_FIX
export KONIX_GIT_ANNEX_PERSO_SYNC_FORCE
export KONIX_GIT_ANNEX_PERSO_SHOW_DATES
export KONIX_GIT_ANNEX_PERSO_SYNC_SKIP_DATA

gaps_log "#### Arguments parsed"
TOP_LEVEL="$(git-toplevel.sh)"
if [ -z "${LIMIT_PATH}" ]
then
    LIMIT_PATH="$(readlink -e .)"
fi
if [ "${LIMIT_PATH}" == "..." ]
then
    LIMIT_PATH="${TOP_LEVEL}"
fi

gaps_log "Top level is ${TOP_LEVEL}"
gaps_log "Limiting to PATH ${LIMIT_PATH}"

pushd "${TOP_LEVEL}"
[ -n "${KONIX_GIT_ANNEX_PERSO_SHOW_DATES}" ] && date
if [ -e "${GITANNEX_BIG_FILE}" ] && [ -n "$KONIX_GIT_ANNEX_PERSO_SYNC_FAST" ]
then
	gaps_log "Skipping due to the presence of ${GITANNEX_BIG_FILE} and option fast set"
	exit 0
fi

OLD_MASTER_SHA=`git log -1 master --pretty=format:"%H"`
OLD_GIT_ANNEX_SHA=`git log -1 git-annex --pretty=format:"%H"`

git-annex-perso_freeze.sh || gaps_error_n_quit "Failure when freezing"
[ -n "${KONIX_GIT_ANNEX_PERSO_SYNC_FIX}" ] && git-annex-perso_remotefix.sh
gaps_log "Annex merging"
git annex merge

if [ -e "${GITANNEXSYNC_POST_HOOK}" ]
then
    DIR=`pwd`
	gaps_log "Setting up the post hook"
	post_hook_launch ( ) {
		gaps_log "Launching the posthook"
		bash -x "${DIR}/${GITANNEXSYNC_POST_HOOK}"
	}
	trap "post_hook_launch" 0
fi

# sync with the remotes
gaps_log "Syncing with remotes"
ARGUMENTS=""
if [ -n "${KONIX_GIT_ANNEX_PERSO_SYNC_SKIP_DATA}" ]
then
    ARGUMENTS="${ARGUMENTS} -s"
fi
if [ -n "${KONIX_GIT_ANNEX_PERSO_SYNC_FORCE}" ]
then
    ARGUMENTS="${ARGUMENTS} -F"
fi

git-annex-perso_syncremotes.sh -r "${SYNC_REMOTE}" ${ARGUMENTS} -p "${LIMIT_PATH}"
[ -n "${KONIX_GIT_ANNEX_PERSO_SHOW_DATES}" ] && date

NEW_MASTER_SHA=`git log -1 master --pretty=format:"%H"`
NEW_GIT_ANNEX_SHA=`git log -1 git-annex --pretty=format:"%H"`
if [ -n "${KONIX_GIT_ANNEX_PERSO_SYNC_SKIP_DATA}" ]
then
    gaps_log "Skipping data transfer"
else
    gaps_log "Limiting to path ${LIMIT_PATH}"
    pushd "${LIMIT_PATH}"
    gaps_log "Data transfer !"
    if [ "${OLD_GIT_ANNEX_SHA}" == "${NEW_GIT_ANNEX_SHA}" \
        -a \
        "${OLD_MASTER_SHA}" == "${NEW_MASTER_SHA}" \
        -a \
        -z "${KONIX_GIT_ANNEX_PERSO_SYNC_FORCE}" ]
    then
        gaps_log "Nothing changed in any branch"
        gaps_log "-> do not try to launch the get/drop commands"
    else
	    if [ -n "${KONIX_GIT_ANNEX_PERSO_SYNC_FORCE}" ]
	    then
		    gaps_log "-> Force set, then force copy/drop"
	    fi
        gaps_log "################## Launching the getting dropping command"
        gaps_log "Auto getting data (only what is not already here)"
        git annex get --fast --auto --not --in here "."
        gaps_log "Auto dropping data (only what is already here)"
        git annex drop --fast --auto --in here "."
        gaps_log "Syncing to make sure everything is alright"
        git annex sync
    fi
    popd
fi

[ -n "${KONIX_GIT_ANNEX_PERSO_SHOW_DATES}" ] && date
popd
