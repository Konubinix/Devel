#!/usr/bin/env bash

git commit -m "fixup! $(git log -1 --format='%s' $@)"
