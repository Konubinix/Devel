#!/bin/bash

MESSAGE="${1:-Freezing of repo}"

source "git-annex-perso_lib.sh"

die ( ) {
    gaps_error "$*"
    exit 1
}

gaps_launch_freeze_pre_hook ( ) {
    local hook="${1}"
    local message="${2}"
    if [ -f "${hook}" ]
    then
        gaps_log "Launching the freeze prehook (${hook}) for $message"
        bash "${hook}" \
            || die "Failed to launch the freeze prehook for ${message}"
    fi
}

gaps_launch_freeze_post_hook ( ) {
    local hook="${1}"
    local message="${2}"
    if [ -f "${hook}" ]
    then
        gaps_log "Launching the freeze posthook (${hook}) for $message"
        bash "${hook}" \
            || die "Failed to launch the freeze posthook for ${message}"
    fi
}
USER="${USER:-user}"
git config user.name "${USER}"
git config user.email "${USER}@${HOSTNAME}"
GITANNEXFREEZE_PRE_HOOK=".gitannexfreeze_prehook"
GITANNEXFREEZE_POST_HOOK=".gitannexfreeze_posthook"

pushd `git -c core.bare=false rev-parse --show-toplevel`
gaps_log "First merge the annex in case someone else pushed the sync branch here"
git annex merge || die "Could not merge"
gaps_launch_freeze_pre_hook "${GITANNEXFREEZE_PRE_HOOK}" "$(pwd)"
gaps_launch_freeze_pre_hook "${GITANNEXFREEZE_PRE_HOOK}_${HOSTNAME}" "$(pwd)"
if [ "$(git config annex.direct)" == "true" ]
then
    gaps_log "In direct mode, I have to make git sync before git annex"
    git -c core.bare=false diff --name-only --diff-filter=M -z | xargs --no-run-if-empty -0 git -c core.bare=false add -v
    gaps_log "Remove from the index files that were deleted in the working tree"
    git -c core.bare=false diff --name-only --diff-filter=D -z | xargs --no-run-if-empty -0 git -c core.bare=false rm --cached || die "Could not remove deleted files"
fi
gaps_log "Annex add new files"
git annex add || die "Could not annex add"
if ! [ "$(git config annex.direct)" == "true" ]
then
	gaps_log "In indirect mode, syncing the working copy to index after git annex"
	git add -A :/ || die "Could not add -A files"
else
	gaps_log "Add files ignored by git annex"
	git -c core.bare=false ls-files --others --exclude-standard -z|xargs -0 --no-run-if-empty git -c core.bare=false add -v
fi
# taken from https://github.com/svend/home-bin/blob/master/git-autocommit
hostname=`hostname`
dnsdomainname=`dnsdomainname 2>/dev/null || true`
if [ -n "$dnsdomainname" ]; then
    hostname="$hostname.$dnsdomainname"
fi

if [ -z "$GIT_COMMITTER_EMAIL" ]; then
    export GIT_COMMITTER_EMAIL=`whoami`"@$hostname"
fi
# all except the type change in direct mode
if [ -n "$(git diff --cached --name-only --diff-filter=ACDMRUXB)" ]
then
    echo "Perform the commit"
    git -c core.bare=false commit -m "${MESSAGE} by $LOGNAME at $HOSTNAME" || die "Could not commit"

    gaps_launch_freeze_post_hook "${GITANNEXFREEZE_POST_HOOK}" "$(pwd)"
    gaps_launch_freeze_post_hook "${GITANNEXFREEZE_POST_HOOK}_${HOSTNAME}" "$(pwd)"

else
    echo "No need to commit"
fi
popd
