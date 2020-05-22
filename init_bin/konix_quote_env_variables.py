#!/usr/bin/env python2
# -*- coding:utf-8 -*-


import re
import sys

for line in sys.stdin.readlines():
        print line.replace("\\","\\\\"),
