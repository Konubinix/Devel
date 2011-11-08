#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import glob
import sys
import re
from konix_fairy_lib import get_files
from optparse import OptionParser

if __name__ == '__main__':
        parser = OptionParser()
        parser.add_option("-a", "--all",
                          dest="all",
                          help="Print all matches",
                          default=False,
                          action="store_true"
                          )
        (options, args) = parser.parse_args()
        ALL=options.all

        files = get_files(args, ALL)
        for fil in files:
                print fil
