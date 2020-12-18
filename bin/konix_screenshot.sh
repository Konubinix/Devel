#!/bin/bash -eu

source _docs.sh

TMP="$(mktemp -d)"
trap "rm -rf '${TMP}'" 0

import "${TMP}/out.png"
CID="/ipfs/$(ipfs add --pin=false --quiet "${TMP}/out.png")"
sql -c "insert into screenshot (cid, date, state, mimetype) values ('${CID}', 'now', 'todo', 'image/png')" >&2
echo -n "${CID}"
konix_display.py "Captured"
