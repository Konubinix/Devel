#!/bin/bash

# https://gist.github.com/lukewilkins/5038960
git config pack.threads 1
git config pack.deltaCacheSize 1
git config core.packedGitWindowSize 16m
git config core.packedGitLimit 128m
git config pack.windowMemory 512m
