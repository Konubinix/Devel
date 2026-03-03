#!/usr/bin/env bash -eu

git annex find --format '${humansize} ${file}\n'|sed 's/ //'|sort -h
