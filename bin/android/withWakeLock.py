#! /system/bin/env python

import subprocess
import sys
import android

droid = android.Android()

print "Acquiring Wakelock"
droid.wakeLockAcquireDim()
ret = subprocess.call(sys.argv[1:])
print "Releasing Wakelock"
droid.wakeLockRelease()
exit(ret)
