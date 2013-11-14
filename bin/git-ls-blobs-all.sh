#!/bin/bash

# taken from
# http://stackoverflow.com/questions/7348698/git-how-to-list-all-objects-in-the-database

cd "$(git rev-parse --git-dir)"

# Find all the objects that are in packs:

find objects/pack -name 'pack-*.idx' | while read p ; do
    git show-index < $p | cut -f 2 -d ' '
done

# And now find all loose objects:
find objects/ \
    | egrep '[0-9a-f]{38}' \
    | perl -pe 's:^.*([0-9a-f][0-9a-f])/([0-9a-f]{38}):\1\2:' \
;
