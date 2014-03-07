#!/bin/bash -x

git-annex-freeze.sh
git-annex sync "$@"
