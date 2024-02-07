#!/bin/bash
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

cat<<EOF > sudo tee /etc/apt/apt.conf.d/01norecommend
APT::Install-Recommends "0";
APT::Install-Suggests "0";
EOF

packages=(
    git git-annex
    awesome
    gnupg gpg-agent libgfshare-bin
    rsync
    pipx
    net-tools tinc
    xinit xdm
)

sudo apt update && sudo apt install "${packages[@]}"
