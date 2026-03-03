#!/usr/bin/env bash

cat "${KONIX_WEB_SEARCH_ENGINES}"|sed -n -e "/^\[$1\]$/{
n
p
}"
