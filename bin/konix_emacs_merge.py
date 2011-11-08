# -*- coding:utf-8 -*-

import sys
import os

local = sys.argv[1].replace('\\','/')
remote = sys.argv[2].replace('\\','/')
base = sys.argv[3].replace('\\','/')
merged = sys.argv[4].replace('\\','/')

eval_string = r'''"emacsclient" -eval '(progn
(ediff-merge-files-with-ancestor "'''+local+'" "'+remote+'" "'+base+'" nil "'+merged+'''")
)'
'''
os.system(eval_string)
