#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
from pathlib import Path
import json

from sqlalchemy import create_engine
from click_project.lib import cd, check_output
import sys


def get_db(db_location, path):
    with cd(db_location.parent):
        return create_engine(f'sqlite:///{path}.db')


def main(db_location, exported_path, kind):
    roam = get_db(db_location, "org-roam")
    exported = [
        f'"{file}"'
        for file in exported_path.read_text().strip().splitlines()
    ]
    links = []
    handled = set()
    nodes = []

    def name(node):
        return node.replace('"', '').replace(f"{os.environ['KONIX_PERSO_DIR']}/roam/", "")[:-len(".org")]

    def add_node(node):
        if node not in handled:
            handled.add(node)
            nodes.append(
                {
                    "id": name(node),
                    "url": f"/{kind}/posts/{name(node)}/",
                    "group": 1
                }
            )

    with roam.connect() as con:
        for src, dest in con.execute("select source, dest from links"):
            if src in exported and dest in exported:
                add_node(src)
                add_node(dest)
                links.append(
                    {
                        "source": name(src),
                        "target": name(dest),
                        "type": "file"
                    }
                )
    print(
        json.dumps(
            {
                "links": links,
                "nodes": nodes,
            }
        )
    )


if __name__ == "__main__":
    main(
        Path(sys.argv[1]),
        Path(sys.argv[2]),
        sys.argv[3],
    )
