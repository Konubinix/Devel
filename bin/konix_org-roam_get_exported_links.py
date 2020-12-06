#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from pathlib import Path
from sqlalchemy import create_engine
from click_project.lib import cd, check_output
import sys


def get_db(db_location, path):
    with cd(db_location.parent):
        return create_engine(f'sqlite:///{path}.db')


def main(db_location, roam_dir, exported_path):
    roam = get_db(db_location, "org-roam")
    exported = [
        f'"{file}"'
        for file in exported_path.read_text().strip().splitlines()
    ]
    with roam.connect() as con:
        for src, dest in con.execute("select source, dest from links"):
            if src in exported and dest in exported:
                print(f"{src}|{dest}")


if __name__ == "__main__":
    main(
        Path(sys.argv[1]),
        Path(sys.argv[2]),
        Path(sys.argv[3]),
    )
