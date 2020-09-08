#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import re
import sys
import argparse

parser = argparse.ArgumentParser(description='Match some regexp in lines.')
parser.add_argument('--quiet', '-q', action='store_true', help='do not print but only return the result')
parser.add_argument('--ignore-case', '-i', action='store_true', help='ignore the case')
parser.add_argument('regexp', nargs="*", help='the regexp to search')
args = parser.parse_args()

QUIET = args.quiet
IGNORE_CASE = args.ignore_case
REGEXP = args.regexp

RES=1

RE_FLAGS=0
if IGNORE_CASE:
    RE_FLAGS |= re.I

for line in sys.stdin.readlines():
    for regexp in REGEXP:
        for match in re.findall(regexp, line, RE_FLAGS):
            RES=0
            if not QUIET:
                print(line, end=' ')
exit(RES)
