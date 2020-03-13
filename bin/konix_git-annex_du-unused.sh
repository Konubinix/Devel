#!/bin/bash -eu

sed -r 's/^.+SHA256E-s([0-9]+)-.+$/\1/' .git/annex/unused | paste -d+ -s - |bc -l
