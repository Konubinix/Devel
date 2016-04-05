#!/bin/bash

echo "sai libwebkit2gtk-3.0-dev"

cd uzbl
rm -rf install
make clean
mkdir -p install/lib/python3.5/site-packages/
export PYTHONPATH+=":$(pwd)/install/lib/python3.5/site-packages/"
make PREFIX=$(pwd)/install install
