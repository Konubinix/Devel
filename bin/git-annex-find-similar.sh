#!/bin/bash -x


find -type l -lname "*$(basename "$(readlink "$1")")*"
