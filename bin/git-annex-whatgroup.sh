#!/bin/bash

set -eu

uuid_compute_from_git_annex () {
    local remote_name="$1"
    git show git-annex:uuid.log|grep "^[^ ]\+ ${remote_name} "|cut -f1 -d$' '
}

uuid_compute_local () {
    local remote_name="$1"
    git config "remote.${remote_name}.annex-uuid"
}

uuid_compute () {
    local remote_name="$1"
    uuid_compute_local "${remote_name}" || \
        uuid_compute_from_git_annex "${remote_name}"
}

remote_name="$1"
uuid="$(uuid_compute "${remote_name}")"
# make sure to take the latest recorded group in case they are several
# values. Use the timestamp to do so
git show git-annex:group.log | \
    grep "^$uuid" | \
    sed -r 's/^([^ ]+) (.+) timestamp=([0-9.]+)s$/\3 \2/' | \
    sort -n | \
    tail -1 | \
    cut -f 2 -d $' '
