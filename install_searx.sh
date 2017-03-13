#!/bin/bash

set -eu
pip2 install --user -e searx
echo "Change the settings"
ec searx/searx/settings.yml
