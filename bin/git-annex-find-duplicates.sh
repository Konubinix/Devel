#!/bin/bash

git annex find --include "*" --format='${key}:${file}\n' \
    | cut -d '-' -f 4- \
    | sort \
    | uniq --all-repeated=separate -w 40 \
    | cut -d ':' -f 2-
