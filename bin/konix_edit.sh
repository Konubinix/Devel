#!/bin/bash
#set -x
FILE_TO_EDIT="$1"
if [ -z "$FILE_TO_EDIT" ]
then
  echo "You did not provide a file">&2
else
  if [ ! -e "$FILE_TO_EDIT" ]
  then
    echo "$FILE_TO_EDIT not found, search for it in the path">&2
    FILE_TO_EDIT_WHICH="$(which "$FILE_TO_EDIT")"
    if [ -e "$FILE_TO_EDIT_WHICH" ]
    then
      FILE_TO_EDIT="$FILE_TO_EDIT_WHICH"
    else
      echo "$FILE_TO_EDIT not found in path, it will be given to your editor as is">&2
    fi
  fi
fi
if [ -n "$FILE_TO_EDIT" ]
then
  "$EDITOR" "$FILE_TO_EDIT"
else
  "$EDITOR"
fi
