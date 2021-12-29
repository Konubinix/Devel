#!/bin/bash -eu

echo -n "not yet"|konix_xclip_in_all.sh
CID="$(konix_screenshot.sh)"
hash="${CID#/*/}"
echo -n "${CID}?a.png"|konix_xclip_in_all.sh

TMP="$(mktemp -d)"
trap "rm -rf '${TMP}'" 0

pushd "${TMP}"
{
    ipfs get "${hash}"
    xclip -selection clipboard -t image/png -i "${hash}"
}
popd
