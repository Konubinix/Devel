#!/usr/bin/env bash -x

git-annex-perso-freeze.sh
git-annex sync "$@"
