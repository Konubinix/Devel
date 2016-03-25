#!/usr/bin/env python
# -*- coding:utf-8 -*-

import re
from pipe import *
import collections
import itertools
import six

bstring = basestring if six.PY2 else str

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

@Pipe
def fs(es, v):
    return [e for e in es if v.lower() in str(e).lower()]
gi = fs

@Pipe
def fsn(es, v):
    return [e for e in es if not v.lower() in str(e).lower()]
gv = fsn

@Pipe
def each(es, function):
    if isinstance(function, bstring):
        function = eval("lambda e: {}".format(function))
    for e in es:
        function(e)

@Pipe
def reverse(es):
    return reversed(es)

@Pipe
def ass(es):
    return [str(e) for e in es]

@Pipe
def mp(es, function):
    if isinstance(function, bstring):
        function = eval("lambda e: {}".format(function))
    return map(function, es)

@Pipe
def ft(iterable, predicate):
    if isinstance(predicate, bstring):
        predicate = eval("lambda e: {}".format(predicate))
    for elem in iterable:
        if predicate(elem):
            yield elem

@Pipe
def last(iterable):
    return list(iterable)[-1]

@Pipe
def uniq(iterable):
    return set(iterable)

@Pipe
def do(iterable, function):
    if isinstance(function, bstring):
        function = "def function(e):\n    " + "    \n".join(function.splitlines())
        exec(function)
    for elem in iterable:
        function(elem)
