#!/bin/bash -eu

setxkbmap fr bepo
exec impass gui "$@"
