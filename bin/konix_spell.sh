#!/bin/bash

# Taken from https://github.com/svend/home-bin/blob/master/spell
# Check spelling of arguments

echo "$@" | aspell -a
