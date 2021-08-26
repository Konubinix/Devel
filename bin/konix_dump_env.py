#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import sys

if len(sys.argv) < 2:
        env = os.environ.keys()
else:
        env = (sys.argv[1],)
for key in env:
        print("ENV :",key)
        for val in os.environ[key].split(os.pathsep):
                print("\t",val)
