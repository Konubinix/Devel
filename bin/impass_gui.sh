#!/usr/bin/env bash
set -eu

# libxdo computes keycodes from the core keymap (the one of the last used
# keyboard, possibly ergol) but sends them through the XTEST device, whose
# keymap is the default one (bepo). Make them agree.
xtest_id="$(xinput list --id-only "Virtual core XTEST keyboard")"
xkbcomp "${DISPLAY}" - 2>/dev/null | xkbcomp -i "${xtest_id}" - "${DISPLAY}" 2>/dev/null

exec konix_impass.py gui "$@"
