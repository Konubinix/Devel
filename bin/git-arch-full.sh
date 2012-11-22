#!/bin/bash

COMMIT="${1:-HEAD}"

cd `git-toplevel.sh`
git archive --format=tar.gz $COMMIT . > `git what-commit.sh $COMMIT`.tar.gz
