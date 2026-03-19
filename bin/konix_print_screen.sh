#!/usr/bin/env bash
set -eu

echo -n "not yet"|konix_xclip_in_all.sh
CID_WITH_EXT="$(konix_screenshot.sh)"
CID="${CID_WITH_EXT%%\?*}"
hash="${CID#/*/}"
echo -n "${CID_WITH_EXT}"|konix_xclip_in_all.sh

TMP="$(mktemp -d)"
trap "rm -rf '${TMP}'" 0

pushd "${TMP}"
{
    ipfs get "${hash}"
    ext="${CID_WITH_EXT##*.}"
    xclip -selection clipboard -t "image/${ext}" -i "${hash}"
}
popd
