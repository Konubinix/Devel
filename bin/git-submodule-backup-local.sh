#!/bin/bash

DIR="$1"
if [ -z "$DIR" ]
then
    echo "You must precise a directory"
    exit 1
fi
DIR="${DIR%/}/"

git submodule foreach _git-clone-or-push.sh "$DIR"
