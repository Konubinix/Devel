#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys

for line in sys.stdin.readlines():
    print(line.replace("\\","\\\\"), end=' ')
