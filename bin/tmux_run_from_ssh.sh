#!/bin/bash

set -eu
shopt -s inherit_errexit
TTYNAME="$(tmux_most_recent_client_tty.sh)"
tty_is_ssh.sh "${TTYNAME}"
