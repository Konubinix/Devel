#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import shutil
from optparse import OptionParser
parser = OptionParser()
parser.add_option("-r", "--recursive",
                  dest="recursive",
                  help="Delete all recursively",
                  default=False,
                  action="store_true"
                  )
parser.add_option("-f", "--force",
                  dest="force",
                  help="Compatibility issue, does nothing",
                  default=False,
                  action="store_true"
                  )
parser.add_option("-v", "--verbose",
                  dest="verbose",
                  help="Print what it does",
                  default=False,
                  action="store_true"
                  )
(options, args) = parser.parse_args()
for to_delete_file in args:
        info = "Removing "+to_delete_file
        if os.path.isdir(to_delete_file):
                info+=" directory"
                if options.recursive:
                        delete_func=shutil.rmtree
                else:
                        delete_func=os.rmdir
        else:
                delete_func=os.remove
        print(info)
        delete_func(to_delete_file)
