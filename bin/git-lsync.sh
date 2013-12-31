#!/bin/bash

git-freeze.sh
git fetch
git rebase && git push
