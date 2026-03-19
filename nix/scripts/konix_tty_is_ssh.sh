#!/usr/bin/env bash

set -eu

TTYNAME="$1"
pid=$(ps -o pid= -t "$TTYNAME" | head -1 | tr -d ' ')
while [ "$pid" -ne 1 ] 2>/dev/null; do
    comm=$(ps -o comm= -p "$pid" 2>/dev/null) || break
    case "$comm" in
        sshd|mosh-server) exit 0 ;;
    esac
    pid=$(ps -o ppid= -p "$pid" | tr -d ' ')
done
exit 1
