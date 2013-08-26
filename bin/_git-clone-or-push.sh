#!/bin/bash

DIR="${1%%/}/"
NAME="$(basename $(pwd))"
REPO="$DIR$NAME"
if [ ! -d "$REPO" ] || [ "`ls $REPO`" != "" ]
then
    echo "cloning"
    git clone --mirror . "$REPO"
else
    echo "pushing"
    git push --mirror . "$REPO"
fi
