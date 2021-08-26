#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import pandas as pd
import os
import functools
import operator
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

def pandas_open_in_browser(df):
    import tempfile
    htmlfile = tempfile.NamedTemporaryFile(delete=False, suffix=".html")
    htmlfile.write(df.to_html().encode("utf-8"))
    htmlfile.close()
    os.system("$BROWSER '{}'".format(htmlfile.name))
    os.unlink(htmlfile.name)
pd.DataFrame.open_in_browser = pandas_open_in_browser


def pd_stripna(input_dataframe):
    return input_dataframe.dropna(axis=0, how="all").dropna(axis=1, how="all")


def pd_read_apache_logs(path):
    import pytz
    from datetime import datetime
    def parse_str(x):
        """
        Returns the string delimited by two characters.

        Example:
        `>>> parse_str('[my string]')`
        `'my string'`
        """
        return x[1:-1]

    def parse_datetime(x):
        '''
        Parses datetime with timezone formatted as:
            `[day/month/year:hour:minute:second zone]`

        Example:
            `>>> parse_datetime('13/Nov/2015:11:45:42 +0000')`
            `datetime.datetime(2015, 11, 3, 11, 45, 4, tzinfo=<UTC>)`

        Due to problems parsing the timezone (`%z`) with `datetime.strptime`, the
        timezone will be obtained using the `pytz` library.
        '''
        dt = datetime.strptime(x[1:-7], '%d/%b/%Y:%H:%M:%S')
        dt_tz = int(x[-6:-3])*60+int(x[-3:-1])

        return dt.replace(tzinfo=pytz.FixedOffset(dt_tz))

    return pd.read_csv(
        path,
        sep=r'\s(?=(?:[^"]*"[^"]*")*[^"]*$)(?![^\[]*\])',
        engine='python',
        na_values='-',
        header=None,
        usecols=[0, 3, 4, 5, 6, 7, 8],
        names=['ip', 'time', 'request', 'status', 'size', 'referer', 'user_agent'],
        converters={'time': parse_datetime,
                    'request': parse_str,
                    'status': int,
                    'size': int,
                    'referer': parse_str,
                    'user_agent': parse_str})


def groupby_split(groupby):
    return [groupby.get_group(x) for x in sorted(groupby.groups)]


def datetime_mean(series):
    dt_min = series.min()
    deltas = [
        x - dt_min
        for x in series
    ]
    return (
        dt_min +
        functools.reduce(operator.add, deltas) / len(deltas))
