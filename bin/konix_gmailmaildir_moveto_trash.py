#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import re
import sys
import shutil

mail=sys.argv[1]
candidates = ['Corbeille','Bin','Trash']
found = False
regexp = "^(%s/Mail/[^/]+)/(\[Gmail\]\.)([^/]+)/(.+)$" % (os.environ["HOME"])
assert(re.match(regexp, mail))
for candidate in candidates:
    path_candidate = re.sub(regexp,
                            r'\1/\2%s/\4' % candidate,
                            mail)
    if os.path.exists(os.path.split(path_candidate)[0]):
        found = True
        break

assert(found)
print "%s -> %s" % (mail, path_candidate)
shutil.move(mail, path_candidate)
