#!/bin/bash

set -eu
TTYNAME="$(tmux_most_recent_client_tty.sh)"
tty_is_ssh.sh "${TTYNAME}"
