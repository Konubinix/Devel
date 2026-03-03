#!/usr/bin/env bash

DATE="$*"

git log --until "$DATE" -n 1 --pretty=format:'%H'
