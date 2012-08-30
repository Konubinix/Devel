#!/bin/bash

# This program is free software. It comes without any warranty, to
# the extent permitted by applicable law. You can redistribute it
# and/or modify it under the terms of the -Do What The Fuck You Want
# To Public License-, Version 2, as published by Sam Hocevar. See
# http://sam.zoy.org/wtfpl/COPYING for more details.

. "$UZBL_UTIL_DIR"/editor.sh

element=`echo 'js document.activeElement.type' | socat -t 1 - unix-connect:"$UZBL_SOCKET" | grep -v EVENT`

[ "$element" != 'text' -a "$element" != 'textarea' ] && exit 0

value=`echo 'js document.activeElement.value' | socat -t 1 - unix-connect:"$UZBL_SOCKET" | grep -v EVENT`

tmp_file=`mktemp /tmp/uzbl_edit.XXXXX`
echo -n "$value" > "$tmp_file"

$UZBL_EDITOR $tmp_file

if [ "$value" != "$(< $tmp_file)" ]
    then
    echo "script @scripts_dir/base64.js" > "$UZBL_FIFO"
    # in case that actelem.type has changed, we do this test
    echo "js var actelem = document.activeElement; if(actelem.type == 'text' || actelem.type == 'textarea') {actelem.value = decode64('`base64 $tmp_file | tr -d '\n'`')};" > "$UZBL_FIFO"
fi

rm -rf $tmp_file
