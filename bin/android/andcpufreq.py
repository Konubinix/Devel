#! /usr/bin/env python3

import android
import os, sys
import subprocess
import konix_fairy_lib as fairy
import andlib

# get available governors
p=subprocess.Popen(["cpufreq-info", "-g"], stdout=subprocess.PIPE)
p.wait()
governors=p.stdout.read().split(" ")
# get available policy
p=subprocess.Popen(["cpufreq-info", "-p"], stdout=subprocess.PIPE)
p.wait()
policy=p.stdout.read()

droid = android.Android()
chooser=andlib.Chooser(droid)
governor=chooser.choose(
        "Select governor\n%s" % policy,
        (
            governors
            )
        )
if not governor:
    sys.exit(1)
askpass=fairy.get_files(("andaskpass.py",), False).pop()
os.environ["SUDO_ASKPASS"]=askpass
p=subprocess.Popen(["sudo", "-A", "cpufreq-set", "-g", governor],
        stderr=subprocess.PIPE,
        env=os.environ)
p.wait()
if p.returncode == 0:
    droid.makeToast("Governor set to %s" % governor)
else:
    andlib.display(droid, "Failed",p.stderr.read())
