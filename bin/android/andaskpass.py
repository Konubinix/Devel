#! /usr/bin/env python

import os
os.environ["AP_HOST"]="127.0.0.1"
os.environ["AP_PORT"]="45001"
import android

droid = android.Android()
print droid.dialogGetPassword().result,
