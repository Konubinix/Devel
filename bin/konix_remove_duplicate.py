#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys


def main():
    cache = set()
    for line in sys.stdin:
        line = line.strip()
        if line not in cache:
            print(line)
            cache.add(line)


if __name__ == "__main__":
    main()
