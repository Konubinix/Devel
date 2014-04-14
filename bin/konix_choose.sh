#!/bin/bash

set -eu
LINE_NUMBER="$1"
head -n $1 |tail -n 1
