#!/bin/bash

find -L "$1" -name "*.org" '!' -path '*data*'|sed 's-/[^/]\+$-/-'|uniq|egrep -v '/data/'
