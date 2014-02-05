#!/bin/bash

echo -n $(assword_dump_clear.sh "$@"|grep password|sed -r 's/^.+"password":.+"(.+)"$/\1/')
