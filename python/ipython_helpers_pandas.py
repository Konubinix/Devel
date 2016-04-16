#!/usr/bin/env python
# -*- coding:utf-8 -*-

import pandas as pd
from pandas import DataFrame as df, Index

idx = pd.IndexSlice
df = pd.DataFrame
pd.options.display.width = 200

def pandas_dump_hdf(df, file_name, prefix_key="", *args, **kwargs):
    for column in df.columns:
        df[column].to_hdf(
            file_name,
            ("/" + prefix_key + "/" + column) if prefix_key else column,
            *args,
            **kwargs
        )

pd.DataFrame.dump_hdf = pandas_dump_hdf
