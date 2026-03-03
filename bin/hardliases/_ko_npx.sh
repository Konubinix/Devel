#!/usr/bin/env bash

set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

function npx_run {
    local package="$1"
    shift
    local stamp_name="${package//\//-}"
    local stamp_file="${XDG_CACHE_HOME:-$HOME/.cache}/${stamp_name}-npx-update"

    if [ -f "$stamp_file" ] && [ "$(date +%F)" = "$(cat "$stamp_file")" ]; then
        npx --prefer-offline "$package" "$@"
    else
        npx "$package@latest" "$@"
        date +%F > "$stamp_file"
    fi
}
