#!/bin/bash

find -L "$1" -name 'data' -prune -or -name "*.org" |sed 's-/[^/]\+$-/-'|sort|uniq
