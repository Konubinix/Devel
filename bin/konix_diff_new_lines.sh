#!/bin/bash

OLD="$1"
NEW="$2"
shift 2
diff "$@" \
    --unchanged-group-format='' \
    --old-group-format='' \
    --new-group-format='%>' \
    "$OLD" "$NEW"
