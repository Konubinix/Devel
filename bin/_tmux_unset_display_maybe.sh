#!/bin/bash

[ "$BASH_SOURCE" == "" ] && {
    echo "Source me"
    exit 1
}

if tmux_run_from_ssh.sh 
then
            unset DISPLAY
fi
