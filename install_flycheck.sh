#!/bin/bash

curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
cd config/elfiles/flycheck
make init
make compile
