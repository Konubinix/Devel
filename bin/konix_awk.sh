#!/usr/bin/env bash
set -eu

set -x
awk -f "$@"
