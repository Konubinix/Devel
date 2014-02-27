#!/bin/bash

set -eu

remote_name="$1"
uuid="$(git config remote.${remote_name}.annex-uuid)"
git show git-annex:group.log|grep "^${uuid}"|cut -d $' ' -f 2
