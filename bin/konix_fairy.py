#!/usr/bin/env python3
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
        parser.add_option("-n", "--no-apropos",
                          dest="no_apropos",
                          help="Remove the automatic .* put before and after the request",
                          default=False,
                          action="store_true"
                          )
        parser.add_option("-e", "--env-variable",
                          dest="env_variable",
                          help="Use this environment variable instead of PATH",
                          default="PATH"
                          )
        (options, args) = parser.parse_args()
        ALL=options.all
        NO_APROPOS=options.no_apropos
        # set the args for apropos matching
        if not NO_APROPOS:
            args = [".*"+arg+".*" for arg in args]
        files = get_files(args, ALL, options.env_variable)
        for fil in files:
                print fil
