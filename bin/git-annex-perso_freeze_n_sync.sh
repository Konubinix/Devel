#!/bin/bash -x

git-annex-perso-freeze.sh
git-annex sync "$@"
