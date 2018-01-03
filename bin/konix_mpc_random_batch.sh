#!/bin/bash

set -x
NUMBER="${1:-100}"
mpc clear
mpc add < <(konix_mpc_beet.sh random -n "${NUMBER}")
mpc consume on
mpc play
