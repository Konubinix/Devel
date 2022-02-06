#!/bin/bash

set -eu

TTYNAME="$1"
who|grep "$TTYNAME"|grep -q -e "ssh\|mosh"
