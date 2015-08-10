#!/bin/bash

git branch -a|grep '^ \+views/'|while read view
do
    git branch -D "${view}"
done
