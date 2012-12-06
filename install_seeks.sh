#!/bin/bash

cd seeks
./autogen.sh
./configure --enable-httpserv-plugin --prefix=$(pwd)/dist --sysconfdir=$(pwd)/dist/etc
make install
