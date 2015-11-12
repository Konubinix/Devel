#!/bin/bash

cd searx
make
echo "Change the settings"
ec searx/settings.yml
