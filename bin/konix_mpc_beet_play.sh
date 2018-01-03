#!/bin/bash -eux

mpc clear
mpc add < <(konix_mpc_beet.sh "$@")
mpc consume on
mpc play
