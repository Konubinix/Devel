#!/usr/bin/env bash
set -eux

mpc clear
mpc add < <(konix_beet_url.sh "$@")
mpc consume on
mpc play
