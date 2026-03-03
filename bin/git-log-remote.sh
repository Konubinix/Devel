#!/usr/bin/env bash
set -x

branch="$1"
shift
git log $@ `git config branch.${branch}.merge`.."${branch}"
