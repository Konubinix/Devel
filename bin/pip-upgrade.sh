#!/bin/bash

python3 -m pip list --outdated --format=freeze| cut -d = -f 1 |\
    xargs -n1 python3 -m pip install -U "$@"
