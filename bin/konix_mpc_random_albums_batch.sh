#!/bin/bash

set -x
NUMBER="${1:-10}"
random ( ) {
    beet random -a -n "${NUMBER}" -f '${id}'| while read id
    do
        konix_beet_url.sh ls "album_id:${id}"
    done
}
mpc clear
mpc add < <(random)
mpc consume on
mpc play
