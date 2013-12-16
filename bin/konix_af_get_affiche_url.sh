#!/bin/bash

ID="$(konix_af.py -i)"
wget \
    -O- \
    --post-data="var0=$*&var2=&var9=&var11=&var15=" \
    "http://atilf.atilf.fr/dendien/scripts/generic/cherche.exe?15;s=${ID};;" \
    > /dev/null
echo "http://atilf.atilf.fr/dendien/scripts/generic/affiche.exe?6;s=${ID};d=1;f=1000;t=1000;r=1;"
