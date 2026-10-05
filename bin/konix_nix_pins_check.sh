#!/usr/bin/env bash

set -eu
shopt -s nullglob

PINS_DIR="$(dirname "$0")/../nix/pins"

for pin in "$PINS_DIR"/*.json
do
    pkg="$(basename "$pin" .json)"
    if nix path-info --store https://cache.nixos.org "$(nix eval --raw "nixpkgs#$pkg.outPath")" > /dev/null 2>&1
    then
        echo "$pkg: can go"
    else
        echo "$pkg: still broken"
    fi
done
