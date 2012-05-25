#!/bin/bash

cd `git-toplevel.sh`
git archive --format=tar.gz HEAD . > arch.tar.gz
