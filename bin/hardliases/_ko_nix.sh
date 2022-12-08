#!/bin/bash

set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

function nix_install_binary {
    local bin_name="$1"
    local bin="${HOME}/.nix-profile/bin/${bin_name}"
    local derivation_name="${2:-${bin_name}}"
    local flake="${3:-nixpkgs}"

    if ! test -e "${bin}"
    then
        nix flake update "${flake}"
        nix profile install "${flake}#${derivation_name}"
    fi
    echo "${bin}"
}
