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

def pandas_mpl_imshow_adjust_ticks(df, x_step=5, y_step=5):
    import matplotlib.pyplot as plt
    plt.xticks(
        range(0, df.shape[0], x_step),
        df.index[0::x_step]
    )
    plt.yticks(
        range(0, df.shape[1], y_step),
        df.columns[0::y_step]
    )

pd.DataFrame.dump_hdf = pandas_dump_hdf

def pd_stripna(input_dataframe):
    return input_dataframe.dropna(axis=0, how="all").dropna(axis=1, how="all")
