#!/bin/bash

if [ -e "${MR_REPO}/.gitannexbackuped" ]
then
    git annex whereis --not --copies=backup:1
else
    echo "Not specified to being backuped"
fi
