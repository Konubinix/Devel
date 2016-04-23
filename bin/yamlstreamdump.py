#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import yaml
import time

print(yaml.dump(
    [
        3, {1:'t'}
    ],
    explicit_start=True,
    explicit_end=True,
    indent=True,
    line_break=True,
))
sys.stdout.flush()
time.sleep(2)
print(yaml.dump(
    [
        'r', {1:'t'}
    ],
    explicit_start=True,
    explicit_end=True,
    indent=True,
    line_break=True,
))

if __name__ == "__main__":
    pass
