#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import argparse
import sys
import os
parser = argparse.ArgumentParser(description="""Canonize a PATH environment variable. It outputs on stdout the version of
PATH given in stdin with duplicates removed and path canalized.""")

if __name__ == "__main__":
    args = parser.parse_args()

    content = sys.stdin.read().strip()
    paths = content.split(os.path.pathsep)
    result = []
    for path in paths:
        canonized_path = os.path.realpath(path)
        if canonized_path not in result:
            result.append(canonized_path)
    sys.stdout.write(os.path.pathsep.join(result))
