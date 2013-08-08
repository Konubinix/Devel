#!/bin/bash

find -L "$1" -name "*.org"|sed 's-/[^/]\+$-/-'|uniq|egrep -v '/data/'
