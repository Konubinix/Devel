#!/usr/bin/env python
# -*- coding:utf-8 -*-

import re
from pipe import *
import collections

@Pipe
def gi(strings, pattern, i=True):
    return [string for string in strings if re.search(pattern, string, re.I if i else 0)]

@Pipe
def giv(strings, pattern, i=True):
    return [string for string in strings if not re.search(pattern, string, re.I if i else 0)]

def find_duplicates(seq):
    return [i[0] for i in collections.Counter(seq).items() if i[1] > 1]
