#!/bin/bash

log ( ) {
    echo "## $*"
}

source konix_assert_var.sh "$KONIX_PERSO_RAW_DIR"
source konix_assert_var.sh "$KONIX_PERSO_DIR"
source konix_assert.sh ! -d "$KONIX_PERSO_DIR"

log "Initializing"
fusermount -u $HOME/usbkey_plain
mkdir $HOME/usbkey_plain
encfs "${KONIX_PERSO_RAW_DIR}" "$HOME/usbkey_plain"

post_hook ( ) {
	log "Post hook"
	fusermount -u $HOME/usbkey_plain && rm -r $HOME/usbkey_plain
}
trap post_hook 0

log "Cloning $HOME/usbkey_plain/perso into $KONIX_PERSO_DIR"
mkdir -p "$KONIX_PERSO_DIR"
git clone "$HOME/usbkey_plain/perso" "$KONIX_PERSO_DIR"
