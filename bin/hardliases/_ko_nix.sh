#!/bin/bash

set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

warn () {
    echo "$*" >&2
}

function nix_install_binary {
    local bin_name="$1"
    local bin="${HOME}/.nix-profile/bin/${bin_name}"
    local derivation_name="${2:-${bin_name}}"
    local flake="${3:-nixpkgs}"

    if ! test -e "${bin}"
    then
        if ! command -v nix > /dev/null
        then
            warn "nix not installed, won't be able to run ${bin_name}"
            exit 1
        fi
        nix flake update "${flake}"
        local path="${flake}#${derivation_name}"
        konix_display.py "Installing ${path} to use ${bin}"
        local before="$(date +%s)"
        nix profile install "${path}"
        local after="$(date +%s)"
        local elapsed="$((after - before))"
        if test ${elapsed} -ge 5
        then
            konix_display.py "Done installing ${path} in ${elapsed}s"
        fi
    fi
    echo "${bin}"
}
