#!/bin/bash

pip list --outdated --format=freeze| cut -d = -f 1  | xargs -n1 pip install -U
