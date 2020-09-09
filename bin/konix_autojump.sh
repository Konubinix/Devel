#!/bin/bash -eu

PROMPT_COMMAND=""

source "${HOME}/.autojump/etc/profile.d/autojump.sh"
autojump "$@"
