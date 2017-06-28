#!/bin/bash

if [ "$(git status --porcelain --ignore-submodules --untracked-files=no)" == "" ]
then
    NEED_STASH=""
else
    NEED_STASH="1"
    git stash
fi

git pull --rebase "$@"

if [ "${NEED_STASH}" == "1" ]
then
    git stash pop
fi
