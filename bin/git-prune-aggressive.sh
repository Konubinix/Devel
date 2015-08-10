#!/bin/bash

# http://stackoverflow.com/questions/7432912/git-cant-find-blob-want-to-get-rid-of-it-from-pack
echo "## You should think about removing the wip branches that may point to old content"
echo "## Remove the annex views"
git-annex-views-remove.sh
echo "## Those branches remain, consider whether they are all up to date"
git branch -a
echo "## Expires the reflog"
git reflog expire --expire=all --expire-unreachable=all --all
echo "## Reconstruct the packs with -A to unpack loose objects"
git repack -Ad
echo "## Prune loose objects"
git gc --aggressive --prune=now
