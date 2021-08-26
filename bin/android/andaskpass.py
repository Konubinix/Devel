#! /usr/bin/env python3

import os
os.environ["AP_HOST"]="127.0.0.1"
os.environ["AP_PORT"]="45001"
import android

droid = android.Android()
print droid.dialogGetPassword().result,
