#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import netrc
import sys
import argparse
parser = argparse.ArgumentParser(description="""Dump netrc information.""")

parser.add_argument('-m','--machine',
                    help="""The machine name""",
                    type=str,
                    required=True)

parser.add_argument('-l','--login',
                    help="""Dump the login""",
                    action="store_true",
                    required=False)

parser.add_argument('-p','--password',
                    help="""Dump the password""",
                    action="store_true",
                    required=False)

if __name__ == "__main__":
    args = parser.parse_args()
    n = netrc.netrc()
    login, _, password = n.authenticators(args.machine)
    if args.login:
        sys.stdout.write(login)
    if args.password:
        sys.stdout.write(password)
