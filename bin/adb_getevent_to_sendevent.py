#!/usr/bin/env python
# coding: utf-8

import sys


def main():
    print("#!/bin/sh")
    for line in sys.stdin:
        if line.startswith("/dev/input"):
            dev, values = line.split(":")
            _, val1, val2, val3 = values.split(" ")
            print(
                f"sendevent {dev} {int(val1, 16)} {int(val2, 16)} {int(val3, 16)}"
            )


if __name__ == "__main__":
    main()
