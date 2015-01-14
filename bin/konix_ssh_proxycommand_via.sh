#!/bin/bash

# use in ssh/config via
#Host github.com
#     ProxyCommand konix_ssh_proxycommand_via.sh proxier destination destination port
ssh -q "$1" nc -q0 "$2" "$3"
