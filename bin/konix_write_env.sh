#!/bin/bash

ENV_FILE_NAME="$1"
if [ -z "$ENV_FILE_NAME" ]
then
    ENV_FILE_NAME="env.sh"
fi
export -p > $ENV_FILE_NAME
echo "cd \"$(pwd)\"" >> $ENV_FILE_NAME
