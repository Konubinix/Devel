#!/bin/bash

declare -A arr

for file in *; do
  [[ -f "$file" ]] || continue

  read cksm _ < <(md5sum "$file")
  if ((arr[$cksm]++)); then
    rm "$file"
  fi
done
