#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import sys
if os.path.isabs(sys.argv[1]):
  sys.exit(0)
else:
  sys.exit(1)
