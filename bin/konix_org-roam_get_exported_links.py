#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sqlite3
import sys
from pathlib import Path

from clk.lib import cd, check_output


def main(db_location, roam_dir, exported_path):
    roam = sqlite3.connect(db_location)
    exported = [
        f'"{file}"' for file in exported_path.read_text().strip().splitlines()
    ]
    for src, dest in roam.execute(
            "select srcnode.file, dstnode.file from links join nodes as srcnode on links.source = srcnode.id join nodes as dstnode on links.dest = dstnode.id"
    ):
        if src in exported and dest in exported:
            print(f"{src}|{dest}")


if __name__ == "__main__":
    main(
        Path(sys.argv[1]),
        Path(sys.argv[2]),
        Path(sys.argv[3]),
    )
