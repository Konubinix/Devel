#!/bin/bash

set -eu
#set -x
PHRASE="$1"
shift
PHRASE="$(echo "${PHRASE}"|sed -r 's/ +/[ \t]*\\(<[^>]\\+>\\)*[ \t]*/g')"
grep -n "${PHRASE}" "$@"
