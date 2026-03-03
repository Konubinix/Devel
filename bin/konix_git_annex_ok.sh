#!/usr/bin/env bash
set -eux

git annex add "$@"
git annex metadata -s state=ok "$@"
