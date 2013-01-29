#!/bin/bash

uri="$1"
if [[ "$uri" =~ "editor://" ]]
then
    filename=`echo $uri|sed 's/.\+#file=\(.\+\)/\1/'`
    ec "$filename"
else
    konix_emacs_open_w3m.sh "$@"
fi
