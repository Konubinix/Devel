#! /system/xbin/env python

import subprocess
import sys
import android

droid = android.Android()

print "Acquiring Wakelock"
droid.wakeLockAcquirePartial()
ret = subprocess.call(sys.argv[1:])
print "Releasing Wakelock"
droid.wakeLockRelease()
exit(ret)
