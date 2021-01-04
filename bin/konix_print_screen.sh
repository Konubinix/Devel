#!/bin/bash -eu

CID="$(konix_screenshot.sh)"

echo -n "${CID}"|konix_xclip_in_all.sh
xclip -selection clipboard -t image/png -i "${CID}"
