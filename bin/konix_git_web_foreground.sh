#!/usr/bin/env bash
set -eux

GIT_DIR="$(git rev-parse --git-dir)"
PIDFILE="${GIT_DIR}/pid"
GIT="$(which git)"
pidproxy "${PIDFILE}" "${GIT}" instaweb "${@}"
