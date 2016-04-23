#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import yaml

buffer = ""
for line in sys.stdin:
    buffer = buffer + line
    if line.strip() == "...":
        print(yaml.load(buffer))
        buffer = ""

if __name__ == "__main__":
    pass
