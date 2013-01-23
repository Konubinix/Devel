#!/bin/bash

git fsck |grep commit| cut -f3 -d' '|xargs git show --summary
