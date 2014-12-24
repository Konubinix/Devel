#!/bin/bash

# This program is free software. It comes without any warranty, to
# the extent permitted by applicable law. You can redistribute it
# and/or modify it under the terms of the -Do What The Fuck You Want
# To Public License-, Version 2, as published by Sam Hocevar. See
# http://sam.zoy.org/wtfpl/COPYING for more details.

. "$UZBL_UTIL_DIR"/editor.sh
element="$1"

[ "$element" != 'text' -a "$element" != 'textarea' ] && exit 0

value="$(echo "$2"|base64 -d)"

tmp_file=`mktemp /tmp/uzbl_edit.XXXXX`
echo -n "$value" > "$tmp_file"

$UZBL_EDITOR $tmp_file

if [ "$value" != "$(< $tmp_file)" ]
    then
    # in case that actelem.type has changed, we do this test
    echo "js page string \"konix_edited_elem.value = Base64.decode('`base64 $tmp_file | tr -d '\n'`');\"" > "$UZBL_FIFO"
    echo 'js page string "konix_edited_elem.style.backgroundColor = konix_edited_elem_old_background_color;"' > "$UZBL_FIFO"
fi

rm -rf $tmp_file
