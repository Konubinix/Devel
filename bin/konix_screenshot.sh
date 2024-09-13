#!/bin/bash -eu

source _docs.sh

TMP="$(mktemp -d)"
trap "rm -rf '${TMP}'" 0

# Take the screenshot in PNG format
out_png="${KONIX_PERSO_DIR}/data/inbox/$(date -Is|tr : -|tr '+' -).png"
import "${out_png}"

# Convert the screenshot to JPG format
out_jpg="${out_png%.png}.jpg"
convert "${out_png}" "${out_jpg}"

# Compare the sizes of the PNG and JPG files, and keep the smallest one
size_png=$(stat -c%s "${out_png}")
size_jpg=$(stat -c%s "${out_jpg}")

if [[ ${size_jpg} -lt ${size_png} ]]; then
  rm "${out_png}"
  out="${out_jpg}"
else
  rm "${out_jpg}"
  out="${out_png}"
fi

# Add to IPFS and output the CID
CID="/ipfs/$(ipfs add --cid-version=1 --pin=false --quiet "${out}")"
echo -n "${CID}"

# Display notification
konix_display.py "Captured"
