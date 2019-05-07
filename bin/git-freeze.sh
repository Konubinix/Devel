#!/bin/bash
#set -x
print () {
	echo "$*" >&2
}
print "# Adding tracked files"
git add -u -v :/
print "# Removing deleted files"
git-rm-deleted-files.sh
print "# Adding untracked files"
git-add-untracked-files.sh
# taken from https://github.com/svend/home-bin/blob/master/git-autocommit
hostname=`hostname`
dnsdomainname=`dnsdomainname 2>/dev/null || true`
if [ -n "$dnsdomainname" ]; then
        hostname="$hostname.$dnsdomainname"
fi

if [ -z "$GIT_COMMITTER_EMAIL" ]; then
        export GIT_COMMITTER_EMAIL=`whoami`"@$hostname"
fi
if LC_ALL=C git status|grep -q "nothing to commit, working tree clean"
then
        echo "Nothing to commit"
else
    git commit -m "Freezing of repo by $LOGNAME at $HOSTNAME"
fi
