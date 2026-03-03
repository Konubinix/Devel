#!/usr/bin/env bash

set -eu
exit 1
TTYNAME="$1"
who|grep "\b${TTYNAME}\b"|grep -q -e "ssh\|mosh\|192.168"
