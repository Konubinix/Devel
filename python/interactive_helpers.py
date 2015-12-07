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

false = False
true = True
f = false
t = true

@Pipe
def uniquify_preserve_order(seq, idfun=None):
    # from http://www.peterbe.com/plog/uniqifiers-benchmark
    # order preserving
    if idfun is None:
        def idfun(x): return x
    seen = {}
    result = []
    for item in seq:
        marker = idfun(item)
        # in old Python versions:
        # if seen.has_key(marker)
        # but in new ones:
        if marker in seen: continue
        seen[marker] = 1
        result.append(item)
    return result
