#!/bin/bash

cd uzbl
rm -rf install
make PREFIX=$(pwd)/install install
