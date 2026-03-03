#!/usr/bin/env bash
set -x


find -type l -lname "*$(basename "$(readlink "$1")")*"
