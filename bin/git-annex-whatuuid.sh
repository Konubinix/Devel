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

remote="$1"
if [ "${remote}" == "here" ]
then
    git config annex.uuid
    exit 0
fi

uuid_compute "${remote}"
