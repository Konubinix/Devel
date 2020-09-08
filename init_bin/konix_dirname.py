#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import sys

sys.stdout.write(os.path.dirname(sys.argv[1]).replace("\\","/"))
