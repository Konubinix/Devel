#!/bin/bash

set -x
NUMBER="${1:-100}"
mpc clear
mpc add < <(beet random -n "${NUMBER}" -f 'beets:track;${id}')
mpc consume on
mpc play
