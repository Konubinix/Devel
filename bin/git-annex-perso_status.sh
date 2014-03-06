#!/bin/bash

log ( ) {
    echo "## $*"
}

git status
git annex status
