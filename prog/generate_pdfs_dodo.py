#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import fnmatch
import os
import re
import subprocess
import shlex

docs = []
for root, dirnames, filenames in os.walk('.'):
  dirnames = [d for d in dirnames if not re.match("^\..+$", d)]
  for filename in fnmatch.filter(filenames, '*.doc*') + \
      fnmatch.filter(filenames, '*.ppt*') + \
      fnmatch.filter(filenames, '*.xls*'):
      docs.append(os.path.join(root, filename))
def guess_pdf(doc):
  return re.sub("(doc|ppt|xls).?$", "pdf", doc)

def task_generate_pdfs():
  for doc in docs:
    pdf = guess_pdf(doc)
    yield {
      'name' : "Generate %s" % pdf,
      'actions': ['konix_libreoffice_convert_pdf.sh "%s"' % doc],
      'file_dep': [doc],
      'targets': [pdf],
    }
