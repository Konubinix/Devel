#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import sys


def main():
    source = sys.argv[1]
    target = sys.argv[2]
    print(os.path.relpath(source, target))


if __name__ == "__main__":
    main()
