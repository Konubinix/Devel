#!/bin/bash

URL="$*"
cd "$KONIX_DOWNLOAD_DIR"
cclive -b -f best --log-file flash_dl.log --exec "konix_display.py 'Dled %n'" "$URL"
 ""
