#!/bin/bash -eu

setxkbmap fr bepo
exec konix_impass.py gui "$@"
