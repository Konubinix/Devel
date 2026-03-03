#!/usr/bin/env bash
set -eu

CID="$(konix_screenshot.sh)"
ec --eval "(konix/org-capture-screenshot \"${CID}\")"
