#!/bin/bash

set -eu
FILE="$1"
NEW="$(mktemp)"
sort "${FILE}" > "${NEW}"
set -x
mv "${NEW}" "${FILE}"
