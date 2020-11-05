#!/bin/bash -eu

CID="$(konix_screenshot.sh)"

echo -n "${CID}"|xclip -i
xclip -selection clipboard -t image/png -i "${CID}"
