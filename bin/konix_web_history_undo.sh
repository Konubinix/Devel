#!/bin/bash

exec konix_web_history.sh "${UZBL_UNDOLIST_FILE:-${XDG_DATA_HOME:-~/.local/share}/uzbl/undolist}"
