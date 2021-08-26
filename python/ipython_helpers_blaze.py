#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import blaze
from odo import odo

import ipython_helpers_pandas as hpd


def blaze_dump_hdf(db, file_name, prefix_key="", *args, **kwargs):
    for field in db.fields:
        odo(db[field], hpd.df).dump_hdf(
            file_name, (prefix_key + "/" + field) if prefix_key else field,
            *args, **kwargs)


blaze.interactive.InteractiveSymbol.dump_hdf = blaze_dump_hdf
