#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import html2text
import sys


def main():
    print(html2text.html2text(
         sys.stdin.read()
    ))


if __name__ == "__main__":
    main()
