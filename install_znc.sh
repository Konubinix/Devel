#!/bin/bash

set -eu
cd znc
git sm update --init --recursive
./autogen.sh
./configure --prefix="$(pwd)/stage" #--enable-debug
konix_makej.sh install
