#!/bin/bash

export GIT_EXTERNAL_DIFF=git-diff-driver.sh

git diff "$@"
