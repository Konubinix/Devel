#!/bin/bash

find -L "$1" \( -name 'data' -or -name ".stversions" \) -prune -or -name "*.org" |sed 's-/[^/]\+$-/-'|sort|uniq
