#!/bin/bash

uri="$1"
filename=`echo $uri|sed 's/.\+#file=\(.\+\)/\1/'`
ec "$filename"
