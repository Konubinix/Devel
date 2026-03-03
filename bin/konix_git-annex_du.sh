#!/usr/bin/env bash
set -eu

git annex find --format '${humansize} ${file}\n'|sed 's/ //'|sort -h
