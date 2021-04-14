#!/bin/bash

git annex find --print0 --metadata state=todo \
	| xargs -0 -n 1 ls --literal -l --directory \
    | grep -v .Archives
