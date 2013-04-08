#!/bin/bash

jquery_test="$(echo 'js jquery_test();'| socat - unix-connect:"$UZBL_SOCKET")"
answer="$(echo "$jquery_test" | awk '
        /^jquery_test$/ {
            while (getline) {
                if ( /^jquery_test_end/ ) exit
                print
            }
        }
    ')"
if [ "$answer" == "NON" ]
then
    # load jquery
    echo 'script @scripts_dir/jquery-1.7.2.min.js'>"$UZBL_FIFO"
fi
