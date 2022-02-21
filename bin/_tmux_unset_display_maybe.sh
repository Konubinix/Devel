#!/bin/bash

[ "$BASH_SOURCE" == "" ] && {
    echo "Source me"
    exit 1
}

if konix_run_from_tmux.sh && tmux_run_from_ssh.sh
then
    unset DISPLAY
fi
