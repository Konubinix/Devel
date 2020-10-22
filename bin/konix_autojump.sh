#!/bin/bash -eu

PROMPT_COMMAND=""

[[ -s "${HOME}/.autojump/etc/profile.d/autojump.sh" ]] && source "${HOME}/.autojump/etc/profile.d/autojump.sh"
[[ -s "/usr/share/autojump/autojump.bash" ]] && source "/usr/share/autojump/autojump.bash"

autojump "$@"
