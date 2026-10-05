#!/usr/bin/env bash
# usage: konix_nix_pins_suggest.sh <flake dir> <rebuild log>

set -eu

PINS_DIR="$(realpath "$(dirname "$0")/../nix/pins")"
FLAKE_DIR="$(realpath "$1")"
log="$2"
previous="$PINS_DIR/../nixpkgs-previous.json"
previous_rev="$(jq -r .rev "$previous")"
current_rev="$(jq -r .nodes.nixpkgs.locked.rev "$FLAKE_DIR/flake.lock")"

culprits="$(awk -v q="'" '
    index($0, "Cannot build " q) { split($0, parts, q); drv = parts[2]; next }
    drv != "" && /Reason: builder failed/ { print drv }
    { drv = "" }
' "$log" | sort -u)"
blocked="$(awk -v q="'" '
    index($0, "Cannot build " q) { split($0, parts, q); drv = parts[2]; next }
    drv != "" && /Reason: .*dependenc(y|ies) failed/ { print drv }
    { drv = "" }
' "$log" | sort -u)"

for drv in $culprits
do
    pkg="$(nix-store --query --binding pname "$drv" 2> /dev/null || true)"
    if [ -z "$pkg" ]
    then
        echo "$drv: no pname, cannot pin"
        continue
    fi
    if ! path="$(nix eval --raw --impure --expr "(import (builtins.fetchTree (builtins.fromJSON (builtins.readFile $previous))) { system = \"x86_64-linux\"; }).\"$pkg\".outPath" 2> /dev/null)"
    then
        echo "$pkg: not a top-level package, cannot pin"
        continue
    fi
    if ! nix path-info --store https://cache.nixos.org "$path" > /dev/null 2>&1
    then
        echo "$pkg: not in the cache at ${previous_rev:0:8} either, cannot pin"
        continue
    fi
    rebuilt=""
    for dependent in $blocked
    do
        if nix-store --query --requisites "$dependent" | grep -qxF "$drv"
        then
            name="$(basename "$dependent" .drv)"
            rebuilt="$rebuilt ${name#*-}"
        fi
    done
    if [ -n "$rebuilt" ]
    then
        echo "$pkg: pinning it also builds these locally:$rebuilt"
    fi
    read -rp "$pkg: builder failed, pin it to ${previous_rev:0:8} (cached)? [y/N] " ans < /dev/tty
    if [[ "$ans" =~ ^[Yy]$ ]]
    then
        jq --arg r "builder failed at ${current_rev:0:8}" '{reason: $r, nixpkgs: .}' "$previous" > "$PINS_DIR/$pkg.json"
        git -C "$PINS_DIR" add "$pkg.json"
        echo "$pkg: pinned, edit the reason in $PINS_DIR/$pkg.json and rebuild"
    fi
done
