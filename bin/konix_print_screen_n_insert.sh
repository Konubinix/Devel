#!/bin/bash

CID="$(konix_screenshot.sh)"
emacsclient -e "(with-current-buffer (window-buffer (selected-window)) (insert \"\n${CID}?a.png\"))"
