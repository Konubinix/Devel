#!/usr/bin/env bash -x


find -type l -lname "*$(basename "$(readlink "$1")")*"
