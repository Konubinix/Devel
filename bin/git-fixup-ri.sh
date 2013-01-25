#!/bin/bash

TREEISH="$1"
shift
git-fixup.sh "$@" && git rebase -i "$@"
