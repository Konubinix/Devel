#!/usr/bin/env python3
# -*- coding:utf-8 -*-

def parse_args():
    import argparse
    parser = argparse.ArgumentParser(description="""Split an sgf file.""")

    parser.add_argument('-i','--input',
                        help="""The sgf input file""",
                        type=str,
                        required=True)

    parser.add_argument('-o','--output',
                        help="""The output directory""",
                        type=str,
                        default=".",
                        required=False)

    return parser.parse_args()

def main():
    # http://gotools.sourceforge.net/sgflib/index.html
    import sgflib
    import os

    args = parse_args()

    games = sgflib.SGFParser(open(args.input, "r").read()).parse()
    for i, game in enumerate(games):
        open(os.path.join(args.output, "{}.sgf").format(str(i).zfill(3)), "w").write(str(game))

if __name__ == "__main__":
    main()
