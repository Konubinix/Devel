#!/bin/bash

echo -n $(impass_dump_clear.sh "$@"|grep password|sed -r 's/^.+"password":.+"(.+)"$/\1/')
