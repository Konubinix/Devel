#!/bin/bash

set -eu
pushd notmuch
make
popd
pip install --user -e notmuch/bindings/python
