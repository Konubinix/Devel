#!/bin/bash -eu

m3u="${1}"
mpc clear
mpc add < <(grep -v '^#' "${m3u}")
mpc consume on
mpc play
