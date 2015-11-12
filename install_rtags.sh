#!/bin/bash

echo "sai libclang-dev libncurse-dev"

cd rtags
git sm init
git sm update
mkdir build
cd build
cmake ..
make
