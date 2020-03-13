#!/bin/bash -eux

git annex add "$@"
git annex metadata -s state=ok "$@"
