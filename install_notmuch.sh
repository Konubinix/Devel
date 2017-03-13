#!/bin/bash

set -eu
pushd notmuch
make
popd
pip2 install --user -e notmuch/bindings/python
