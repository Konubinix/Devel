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
    local cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/ko-npx/${stamp_name}"
    local bin="${NPX_BIN:-${package##*/}}"
    bin="${bin%@*}"

    if [ -f "$stamp_file" ] && [ "$(date +%F)" = "$(cat "$stamp_file")" ]; then
        npx --yes --prefer-offline --cache "$cache_dir" --package "$package" env -u npm_config_cache -u npm_config_prefer_offline -u npm_config_package "$bin" "$@"
    else
        date +%F > "$stamp_file"
        rm -rf "$cache_dir/_cacache"
        npx --yes --cache "$cache_dir" --package "$package" env -u npm_config_cache -u npm_config_prefer_offline -u npm_config_package "$bin" "$@"
    fi
}
