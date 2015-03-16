#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import konix_gcalib, sys

import argparse
parser = argparse.ArgumentParser(description="""Gcalendar.""")

parser.add_argument('-m','--make-place',
                    help="""Set this option for new events to move existing
                    ones, this is very intrusive but prevents any conflicts of appointment.""",
                    action="store_true")

parser.add_argument('-a','--account',
                    help="""TODO.""",
                    type=str,
                    default="")

parser.add_argument('--event-formatter',
                    help="""TODO.""",
                    type=str)

parser.add_argument('argv', nargs=argparse.REMAINDER)
args = parser.parse_args()

if __name__ == "__main__":
    program = konix_gcalib.GCall(make_place=args.make_place, account=args.account)
    if args.event_formatter:
        program.event_formatter = args.event_formatter
    if len(args.argv) > 0:
        command_line = args.argv[0]
        if len(args.argv) > 1:
            command_line += ' "' + '" "'.join(args.argv[1:]) + '"'
        program.onecmd(command_line)
    else:
        program.cmdloop()
