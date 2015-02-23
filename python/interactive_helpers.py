#!/usr/bin/env python
# -*- coding:utf-8 -*-

import re
from pipe import *

@Pipe
def gi(strings, pattern, i=True):
    return [string for string in strings if re.search(pattern, string, re.I if i else 0)]

@Pipe
def giv(strings, pattern, i=True):
    return [string for string in strings if not re.search(pattern, string, re.I if i else 0)]
