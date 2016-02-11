#!/bin/bash

set -eu

DIR="${1:-$(pwd)}"

exec fsserve -v -t ftp "${DIR}" -a 0.0.0.0 -p 2121
