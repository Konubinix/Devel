#!env python
# -*- coding:utf-8 -*-
"""Simple script used to do launch a diff using emacs
"""


import subprocess
import sys
import tempfile                 # For emacsclient to wait on a temp file
import os                       # to remove temporary file
import time                     # To sleep

FILE1 = sys.argv[1].replace('\\','/')
FILE2 = sys.argv[2].replace('\\','/')
TMPFILE = tempfile.NamedTemporaryFile(delete=False)
TMPFILE.close()
os.remove(TMPFILE.name)
TMPFILENAME = TMPFILE.name.replace('\\','/')

#f = open("c:/temp/test", 'w')
EVAL = r'''
(progn
(konix/ediff-files-properly "'''+FILE1+'" "'+FILE2+'" "'+TMPFILENAME+'''")
)
'''
PROCESS = subprocess.Popen(["emacsclient", "-eval", EVAL])#,stdout=f)
PROCESS.wait()
#f.close()
while not os.path.exists(TMPFILE.name):
    time.sleep(1)

os.remove(TMPFILE.name)
