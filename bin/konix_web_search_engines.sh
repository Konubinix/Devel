#!/bin/bash

cat "${KONIX_WEB_SEARCH_ENGINES}"|sed -n -e '/^\[.\+\]$/{
s/^\[\(.\+\)\]$/\1/ p
}'
