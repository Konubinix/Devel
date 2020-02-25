#!/bin/bash -eu

usage () {
    cat<<EOF
$0 arguments

Description
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

CURBRANCH="$(git rev-parse --abbrev-ref HEAD)"
UPSTREAM="$(git config branch.${CURBRANCH}.upstream)"
BASE="$(git merge-base "${UPSTREAM}" HEAD)"
git rebase -i "${BASE}"
