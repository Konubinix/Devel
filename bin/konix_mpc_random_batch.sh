#!/bin/bash

NUMBER="${1:-100}"
mpc clear
mpc add < <(beet random -n "${NUMBER}" -f 'beets:track;${id}')
mpc play
