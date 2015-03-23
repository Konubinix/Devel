#!/bin/bash

args=""
args="${1}"
shift
for arg in "$@"
do
    args="${args} AND ${arg}"
done
xapers view "${args}"
