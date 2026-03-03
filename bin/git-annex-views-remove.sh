#!/usr/bin/env bash

git branch -a|grep '^ \+views/'|while read view
do
    git branch -D "${view}"
done
