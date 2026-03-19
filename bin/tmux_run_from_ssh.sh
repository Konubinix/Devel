#!/usr/bin/env bash

set -eu
shopt -s inherit_errexit
TTYNAME="$(tmux_most_recent_client_tty.sh)"
konix_tty_is_ssh "${TTYNAME}"
