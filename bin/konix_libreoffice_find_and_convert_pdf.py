#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import fnmatch
import os
import re
import subprocess
import shlex

matches = []
for root, dirnames, filenames in os.walk('.'):
  dirnames = [d for d in dirnames if not re.match("^\..+$", d)]
  for filename in fnmatch.filter(filenames, '*.doc*') + \
      fnmatch.filter(filenames, '*.ppt*') + \
      fnmatch.filter(filenames, '*.xls*'):
      matches.append(os.path.join(root, filename))
def guess_pdf(doc):
  return re.sub("(doc|ppt|xls).?$", "pdf", doc)

to_be_created_doc = [doc for doc in matches if not os.path.exists(guess_pdf(doc))]
if len(to_be_created_doc) == 0:
  print "Nothing to do"
for doc in to_be_created_doc:
  print "-- Converting %s" % doc
  subprocess.call(shlex.split("konix_libreoffice_convert_pdf.sh '%s'" % doc))
