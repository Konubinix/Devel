#!/usr/bin/env bash

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
    local extra="${4-}"
    local stamp="${KONIX_HARDLIASES_STAMP_DIR}/${bin_name}"
    local registry stamped=""
    registry="$(readlink -f /etc/nix/registry.json || true)"
    if test -s "${stamp}"
    then
        read -r stamped < "${stamp}" || true
    fi

    if ! test -e "${bin}" || test "${registry}" != "${stamped}"
    then
        if test -e "${bin}"
        then
            case "$(readlink "${bin}")" in
                *-home-manager-path/*)
                    warn "${bin_name} comes from home-manager, the hardlias is redundant: drop one of them"
                    echo "${bin}"
                    return 0
                    ;;
            esac
        fi
        if ! command -v nix > /dev/null
        then
            warn "nix not installed, won't be able to run ${bin_name}"
            exit 1
        fi
        if [[ "${flake}" == .* || "${flake}" == /* ]]
        then
            nix flake update --flake "${flake}"
        fi
        local path="${flake}#${derivation_name}"
        notify-send "Installing ${path} to use ${bin}"
        local before="$(date +%s)"
        if test -e "${bin}"
        then
            # nix names the element after the last attrpath component, except
            # for default, where it uses the flake subdir (eg flakes/argdown)
            local element="${derivation_name##*.}"
            if test "${derivation_name}" = default
            then
                element="flakes/$(basename "${flake}")"
            fi
            warn "substitute ${element} with a more uptodate version (${bin}, ${registry}, ${stamp}, ${stamped})"
            if ! nix profile remove "${element}"
            then
                # deliberately not stamping: ${bin} is still the build made
                # against the previous registry, so recording the current one
                # would claim it is uptodate and stop us ever retrying. Leave
                # the stamp stale so the next call attempts the upgrade again.
                warn "could not remove ${element}, keeping the current ${bin_name}"
                echo "${bin}"
                return 0
            fi
        fi
        nix profile add ${extra} "${path}"
        local after="$(date +%s)"
        local elapsed="$((after - before))"
        if test ${elapsed} -ge 5
        then
            notify-send "Done installing ${path} in ${elapsed}s"
        fi
        mkdir -p "$(dirname "${stamp}")"
        printf '%s\n' "${registry}" > "${stamp}"
    fi
    echo "${bin}"
}
