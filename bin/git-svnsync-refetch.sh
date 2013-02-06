#!/bin/bash

# When using git svn with a repository A that is synced from another repository
# B via svnsync, git may be confused and add commits from the user svnsync. The
# svnsync command is generally called in the postcommand hook of B.

# The problem of the user svnsync is due to the fact svnsync uses two commands
# to perform the sync:

# 1. It first commits the changes from B to A via the user svn sync doing
# something like: svn ci --username svnsync. The svn pre-commit-hook and
# post-commit-hook are normally called here.

# 2. It then uses a command to change the revprops of the commits made in A to
# make them resemble the ones in B. The svn pre-rev-prop and post-rev-prop are
# called here.

# See http://www.pollinimini.net/blog/svnsync-postcommit/ for more information
# about this behavior.

# The problem arises when git-svn fetches A between 1 and 2. It might happen if
# the post-commit-hook or the pre-revprop-hook hang.

# Because git-svn does not re check svn for old commits, the git history
# contains stuff with the committer being
# Committer: svnsync <svnsync@uuid-of-the-svn-repo>.

# This script finds the parent of the last commit made by svnsync, revert the
# trunk to that destination, delete the association map between the svn
# revisions and the git shas and then relaunches the fetch. The trunk should
# then be in good shape.

# LIMITATIONS: This script assumes that you are on a chechout under the bad
# trunk. It also does not fix branches and tags.

LAST_BAD_COMMIT=`git log --author=svnsync --pretty=oneline |tail -1|cut -f1 -d' '`
[ -n "$LAST_BAD_COMMIT" ] || {
    echo "The trunk is already clean"
    exit 0
}
LAST_GOOD_TRUNK=`git show --pretty=oneline $LAST_BAD_COMMIT^ |head -1|cut -f1 -d' '`
[ -n "$LAST_GOOD_TRUNK" ] || {
    echo "Unable to find the parent of the last bad commit $LAST_BAD_COMMIT"
    exit 1
}
TOPLEVEL=`git rev-parse --show-toplevel`
SVN_FETCH_REV=`git config svn-remote.svn.fetch | cut -f2 -d':'`
[ -n "$SVN_FETCH_REV" ] || {
    echo "Unable to find the svn remote fetch location (something like .git/svn/refs/remotes/trunk)"
    exit 1
}

cd "$TOPLEVEL"
rm ".git/svn/${SVN_FETCH_REV}/.rev_map"*
echo "$LAST_GOOD_TRUNK" > ".git/${SVN_FETCH_REV}"
git svn fetch
