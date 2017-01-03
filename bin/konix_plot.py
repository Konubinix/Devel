#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import argparse
import pandas
import blaze
from pylab import plt

parser = argparse.ArgumentParser(
    description=u"""Plot a source data file using blaze and pandas.

With CSV files, the most used args are
-a index-col=int:0
-a parse-dates
-a delimiter=,

ODO help:

""" + blaze.odo.__doc__,
    formatter_class=argparse.RawDescriptionHelpFormatter
)

parser.add_argument('-a', '--args',
                    nargs="+",
                    help="""Arguments to give to odo""",
                    type=str,
                    required=False)
parser.add_argument('-i', '--ipython',
                    help="""Run ipython immediately after loading the CSV.

Data contains the data""",
                    action="store_true",
                    required=False)
parser.add_argument('source', metavar="SOURCE", type=str,
                    help='The source data to show',)

if __name__ == "__main__":
    args = parser.parse_args()
    kwargs = {}
    for arg in args.args:
        if "=" in arg:
            key, value = arg.split("=")
            if value.startswith("int:"):
                value = int(value[len("int:"):])
            kwargs[key] = value
        else:
            kwargs[arg] = True
    data = blaze.odo(
        args.source,
        pandas.DataFrame,
        **kwargs
    )
    if args.ipython:
        import IPython
        dict_ = globals()
        dict_.update(locals())
        IPython.start_ipython(argv=[], user_ns=dict_)

    data.plot()
    plt.show()
