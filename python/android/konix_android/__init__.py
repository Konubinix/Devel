import android
import os

try:
    droid = android.Android()
except:
    os.system("sl4a.sh")
    droid = android.Android()
    droid.makeToast("Started new sl4a instance")
__all__ = ["droid",]
