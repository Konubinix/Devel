#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import re
import sys
import shutil

mail=sys.argv[1]
candidates = ['Tous les messages','All Mail']
found = False
for candidate in candidates:
    path_candidate = re.sub('^(%s/Mail/[^/]+)/(\[Gmail\]\.)([^/]+)/(.+)$' % (os.environ["HOME"]),
                            r'\1/\2%s/\4' % candidate,
                            mail)
    if os.path.exists(os.path.split(path_candidate)[0]):
        found = True
        break

assert(found)
print "%s -> %s" % (mail, path_candidate)
shutil.move(mail, path_candidate)
