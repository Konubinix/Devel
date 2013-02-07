#!/bin/bash

TREEISH="$1"
shift
echo "Commit the fixup">&2
git-fixup.sh "$TREEISH"
RES="$?"
if [ "$RES" == "0" ]
then
    # now, do the ri
    if git status --porcelain|grep -q ' M'
    then
        # stash before rebasing
        echo "Stashing local changes">&2
        git stash
        STASHED=1
    fi
    echo "Performing the rebase">&2
    git rebase -i "$@"
    if [ "$STASHED" == "1" ]
    then
        echo "Popping the previously stashed changes">&2
        git stash pop
    fi
fi
