#!/bin/bash -eu

source _docs.sh

TMP="$(mktemp -d)"
trap "rm -rf '${TMP}'" 0

import "${TMP}/out.png"
CID="$(ipfs add --pin=false --quiet "${TMP}/out.png")"
sql -c "insert into screenshots (cid, date, state) values ('${CID}', 'now', 'todo')" >&2
echo -n "/ipfs/${CID}"
