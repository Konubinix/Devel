#!/bin/bash

set -eu

DIR="${1:-$(pwd)}"

exec fsserve -v -t http "${DIR}" -a 0.0.0.0 -p 9642
