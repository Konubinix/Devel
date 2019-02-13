#!/bin/bash

set -eu
cd znc
git sm update --init --recursive
git clean -fd
./autogen.sh
./configure --prefix="$(pwd)/stage" #--enable-debug
konix_makej.sh install
