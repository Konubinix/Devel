#!/bin/bash

pushd config/elfiles/ledger-mode
mkdir build
cd build
cmake ..
make
popd
