#!/bin/bash

cd qutebrowser
pip3 install --user -e .[all]
./scripts/asciidoc2html.py
