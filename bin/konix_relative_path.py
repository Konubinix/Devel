#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import sys
source=sys.argv[1]
target=sys.argv[2]
print os.path.relpath(source, target)
