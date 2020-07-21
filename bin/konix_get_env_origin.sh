#!/bin/bash -eu

konix_get_env.py 2>&1 > /dev/null | konix_get_env_origin.awk "$@"
