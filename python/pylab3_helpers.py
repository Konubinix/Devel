#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import pandas
import pandas as pd
idx = pandas.IndexSlice
df = pandas.DataFrame
from pylab import *
try:
    import blaze
except:
    print("Could not import blaze")

try:
    import odo
    from odo_helpers import *
except:
    print("Could not import odo")
