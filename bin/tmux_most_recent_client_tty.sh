#!/bin/bash

set -o pipefail
set -eu
tmux lsc -F '#{client_activity}:#{client_tty}'|sort --reverse |head -1|cut -d: -f2|cut -d/ -f 3-
