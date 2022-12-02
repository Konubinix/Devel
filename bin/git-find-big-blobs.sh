#!/bin/bash

set -eu
# http://naleid.com/blog/2012/01/17/finding-and-purging-big-files-from-git-history
echo "I will analyze pack files, don't forget to run git repack" >&2
git verify-pack -v .git/objects/pack/pack-*.idx | egrep "^\w+ blob\W+[0-9]+ [0-9]+ [0-9]+$" | sort -k 3 -n

