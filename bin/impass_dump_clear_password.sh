#!/bin/bash -eu

PASSWORD="$1"

echo -n $(impass_dump_clear.sh "$PASSWORD"|jq -r ".[\"${PASSWORD}\"].password")
