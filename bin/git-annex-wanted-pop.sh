#!/bin/bash

REMOTE="$1"

wanted="$(git annex wanted "${REMOTE}")"
new_wanted="$(echo "${wanted}"|sed -r 's/^include=[^ ]+ or \( (.+) \)/\1/')"
echo "Old wanted = ${wanted}"
echo "New wanted = ${new_wanted}"
git annex wanted "${REMOTE}" "${new_wanted}"
