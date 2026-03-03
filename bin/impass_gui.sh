#!/usr/bin/env bash
set -eu

setxkbmap fr bepo
exec konix_impass.py gui "$@"
