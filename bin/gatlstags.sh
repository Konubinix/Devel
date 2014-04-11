#!/bin/bash

cd `git rev-parse --show-toplevel`
git annex metadata|grep "^  "|grep -v lastchanged|sort|uniq -c|sort -n
