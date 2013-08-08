#!/bin/bash
#set -x
print () {
	echo "$*" >&2
}
print "# Adding tracked files"
git add -u -v
print "# Removing deleted files"
git-rm-deleted-files.sh
print "# Adding untracked files"
git-add-untracked-files.sh
git commit -m "Freezing of repo by $LOGNAME at $HOSTNAME"
