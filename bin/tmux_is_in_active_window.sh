#!/bin/bash
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

test "$(tmux display-message -p '#{window_index}')" == "$KONIX_TMUX_WINDOW"
