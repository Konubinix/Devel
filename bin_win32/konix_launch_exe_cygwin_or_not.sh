#!/bin/bash
#set -x

COMMAND="$(basename "$1")"
shift 1 # to have only the arguments of the launcher, not the command

# make the assumption the cygwin binaries are
COMMAND_CYG="$(which -a "${COMMAND}"|grep cygwin|head -1)"
COMMAND_MKSNT="$(which -a "${COMMAND}"|grep mksnt|head -1)"
if [ -z "$COMMAND_MKSNT" -a -z "$COMMAND_CYG" ]
then
  echo "Unable to find a cygwin or mksnt installation of $1"
  exit 1
fi
# If there is cygdrive in one of the arguments, suppose it is for cygwin implementation
if echo "$*" |grep -iq "cygdrive"
then
    "$COMMAND_CYG" "$@"
else
    "$COMMAND_MKSNT" "$@"
fi
