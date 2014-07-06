#!/bin/bash

URI="${1:-${UZBL_URI}}"
echo -n "$URI" | konix_edit_stdin.sh
