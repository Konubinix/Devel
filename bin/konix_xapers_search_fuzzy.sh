#!/bin/bash

args=""
args="${1}"
shift
for arg in "$@"
do
    args="${args} NEAR ${arg}"
done
xapers search "${args}"
