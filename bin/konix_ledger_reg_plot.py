#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import pandas
import matplotlib
import subprocess
import shlex

import argparse
parser = argparse.ArgumentParser(description="""Plot some financial data from
ledger.
Running ledger reg -J <account>.

Extra arguments are given to ledger reg command
""")

parser.add_argument("-a", '--account',
                    nargs="+",
                    help="""The accounts to plot, may be used several times.""",
                    type=str,
                    required=True)

def get_dataframe_for_account(account_name, reg_args=None):
    reg_args = reg_args or []
    command = ["ledger", "reg", "-J"] + reg_args + [account_name]
    p = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        universal_newlines=True,
    )
    p.wait()

    assert p.returncode == 0, "'{}' did not end normally".format(command)
    content = pandas.DataFrame(
        [
            line.strip().split(" ")
            for line in p.stdout.readlines()
        ],
        columns=["Date", account_name]
    )

    content.index = pandas.to_datetime(content["Date"])
    content = content.drop("Date", axis=1)
    content = content.astype("f")
    return content

if __name__ == "__main__":
    args, reg_args = parser.parse_known_args()
    account = args.account[0]
    axes = get_dataframe_for_account(account, reg_args).plot()
    for account in args.account[1:]:
        get_dataframe_for_account(account, reg_args).plot(ax=axes)

matplotlib.pyplot.show()
