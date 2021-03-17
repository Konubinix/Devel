#!/bin/bash

git annex find --print0 --metadata state=next \
	| xargs -0 -n 1 ls --literal -l --directory
