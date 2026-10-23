#!/usr/bin/env bash
# [[id:2710b21a-6789-4e35-8fd6-10e3f1862522::konix_nix_pins_check][konix_nix_pins_check]]
set -eu
shopt -s nullglob

PINS_DIR="${KONIX_DEVEL_DIR}/nix/pins"

for pin in "$PINS_DIR"/*.json
do
    pkg="$(basename "$pin" .json)"
    json="(builtins.fromJSON (builtins.readFile $(realpath "$pin")))"
    reason="$(nix eval --raw --impure --expr "$json.reason or \"no reason given\"")"
    if [ "$(nix eval --raw --impure --expr "$json.check or \"cache\"")" = manual ]
    then
        echo "$pkg: check by hand ($reason)"
    elif current="$(nix eval --raw "nixpkgs#$pkg.outPath")" \
            && nix path-info --store https://cache.nixos.org "$current" > /dev/null 2>&1
    then
        echo "$pkg: can go ($reason), to take it into account:"
        rev="$(jq -r .nixpkgs.rev "$pin")"
        echo "    pinned:  $(nix eval --raw "github:NixOS/nixpkgs/$rev#$pkg.outPath")"
        echo "    nixpkgs: $current"
        echo "    git -C ${KONIX_DEVEL_DIR} rm nix/pins/$pkg.json"
        flake=nixpkgs
        {
            if nix-store -qR /run/current-system | grep -q -- "-$pkg-[0-9]"
            then
                echo "konix_nixos-rebuild.sh"
            fi
            if nix-store -qR "${HOME}/.local/state/nix/profiles/home-manager" | grep -q -- "-$pkg-[0-9]"
            then
                echo "konix_home-manager-rebuild.sh"
            fi
            if test -e "${KONIX_DEVEL_DIR}/bin/hardliases/$pkg" \
                    || grep -sqE "nix_install_binary .*\b$pkg\b" "${KONIX_DEVEL_DIR}"/bin/hardliases/*
            then
                echo "nothing to do, the next hardlias call will reinstall it"
            elif nix profile list --json | jq -e --arg p "$pkg" '.elements[$p]' > /dev/null
            then
                echo "nix profile remove $pkg && nix profile add $flake#$pkg"
            fi
        } | sed 's/^/    then /'
    else
        echo "$pkg: still broken ($reason)"
    fi
done
# konix_nix_pins_check ends here
