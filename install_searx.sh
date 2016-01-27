#!/bin/bash

pip install --user -e searx
echo "Change the settings"
ec searx/searx/settings.yml
