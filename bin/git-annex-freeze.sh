#!/bin/bash

log ( ) {
    echo "## $*"
}

die ( ) {
    log "$*"
    exit 1
}

pushd `git rev-parse --show-toplevel`
log "First merge the annex in case someone else pushed the sync branch here"
git annex merge || die "Could not merge"
log "Annex add new files"
git annex add || die "Could not annex add"
if [ "$(git config annex.direct)" == "true" ]
then
    echo "direct mode, so do not git freeze"
    echo "Removing from the index files that were deleted in the working tree"
    git diff --name-only --diff-filter=D -z | xargs -0 git rm --cached || die "Could not remove deleted files"
else
	echo "Syncing working copy to index"
	git add -A :/ || die "Could not add -A files"
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
if [ -n "$(git status --porcelain)" ]
then
    echo "Perform the commit"
    git commit -m "Freezing of repo by $LOGNAME at $HOSTNAME" || die "Could not commit"
else
    echo "No need to commit"
fi
popd
