#!/bin/bash

echo "sai libwebkit2gtk-3.0-dev"

cd uzbl
rm -rf install
make PREFIX=$(pwd)/install install
