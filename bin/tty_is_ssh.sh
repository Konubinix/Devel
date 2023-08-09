#!/bin/bash

set -eu

TTYNAME="$1"
who|grep "\b${TTYNAME}\b"|grep -q -e "ssh\|mosh\|192.168"
