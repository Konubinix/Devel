#!/usr/bin/env bash

log ( ) {
    echo "## $*"
}

git status
git annex status
