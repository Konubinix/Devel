#!/bin/bash -eu

source _docs.sh

TMP="$(mktemp -d)"
trap "rm -rf '${TMP}'" 0

out="${KONIX_PERSO_DIR}/data/inbox/$(date -Is|tr : -|tr '+' -).png"
import "${out}"

CID="/ipfs/$(ipfs add --cid-version=1 --pin=false --quiet "${out}")"
echo -n "${CID}"
konix_display.py "Captured"
