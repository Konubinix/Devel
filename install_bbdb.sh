#!/bin/bash

set -eu
cd config/elfiles/bbdb
./autogen.sh
./configure
make
