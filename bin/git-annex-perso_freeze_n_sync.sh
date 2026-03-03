#!/usr/bin/env bash
set -x

git-annex-perso-freeze.sh
git-annex sync "$@"
