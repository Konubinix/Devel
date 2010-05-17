#!/usr/bin/python

import pyosd
import sys, os
import time
if len(sys.argv) <= 1:
    exit(1)

if os.path.isfile('/tmp/display'):
    fd = open('/tmp/display','r')
    pid = fd.readline()
    fd.close()
    os.system("kill -9 "+str(pid))

pid = os.getpid()

p = pyosd.osd("-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1", colour="green", pos=pyosd.POS_BOT,offset=40, align=pyosd.ALIGN_CENTER)
p.display(' '.join(sys.argv[1:]))

os.system('echo '+str(pid)+' > /tmp/display')
time.sleep(1)
os.remove('/tmp/display')
