#!/bin/bash

# http://stackoverflow.com/questions/7432912/git-cant-find-blob-want-to-get-rid-of-it-from-pack
echo "Expires the reflog"
git reflog expire --expire-unreachable=now --all
echo "Reconstruct the packs with -A to unpack loose objects"
git repack -Ad
echo "Prune loose objects"
git gc --aggressive --prune=now
