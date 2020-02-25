#!/bin/bash

set -eu
pip3 install --user -e searx
echo "Change the settings"
ec searx/searx/settings.yml
