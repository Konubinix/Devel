#!/usr/bin/env bash

git-freeze.sh
git fetch
git rebase && git push
