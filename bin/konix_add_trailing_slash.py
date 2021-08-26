#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import re

if not re.search("/$",sys.argv[1]):
        print sys.argv[1] + "/"
else:
        print sys.argv[1]
